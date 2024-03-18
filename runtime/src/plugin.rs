use std::{
    collections::{BTreeMap, BTreeSet},
    path::PathBuf,
};

use crate::*;

pub const EXTISM_ENV_MODULE: &str = "extism:host/env";
pub const EXTISM_USER_MODULE: &str = "extism:host/user";
pub(crate) const MAIN_KEY: &str = "main";

#[derive(Default, Clone)]
pub(crate) struct Output {
    pub(crate) offset: u64,
    pub(crate) length: u64,
    pub(crate) error_offset: u64,
    pub(crate) error_length: u64,
}

/// A `CancelHandle` can be used to cancel a running plugin from another thread
#[derive(Clone)]
pub struct CancelHandle {
    pub(crate) timer_tx: std::sync::mpsc::Sender<TimerAction>,
    pub id: uuid::Uuid,
}

unsafe impl Sync for CancelHandle {}
unsafe impl Send for CancelHandle {}

impl CancelHandle {
    pub fn cancel(&self) -> Result<(), Error> {
        debug!(plugin = self.id.to_string(), "sending cancel event");
        self.timer_tx.send(TimerAction::Cancel { id: self.id })?;
        Ok(())
    }
}

/// Plugin contains everything needed to execute a WASM function
pub struct Plugin {
    /// A unique ID for each plugin
    pub id: uuid::Uuid,

    /// Wasmtime linker
    pub(crate) linker: Linker<CurrentPlugin>,

    /// Wasmtime store
    pub(crate) store: Store<CurrentPlugin>,

    /// A handle used to cancel execution of a plugin
    pub(crate) cancel_handle: CancelHandle,

    /// All modules that were provided to the linker
    pub(crate) modules: BTreeMap<String, Module>,

    /// Instance provides the ability to call functions in a module, a `Plugin` is initialized with
    /// an `instance_pre` but no `instance`. The `instance` will be created during `Plugin::raw_call`
    pub(crate) instance: std::sync::Arc<std::sync::Mutex<Option<Instance>>>,
    pub(crate) instance_pre: InstancePre<CurrentPlugin>,

    /// Keep track of the number of times we're instantiated, this exists
    /// to avoid issues with memory piling up since `Instance`s are only
    /// actually cleaned up along with a `Store`
    instantiations: usize,

    /// Runtime determines any initialization functions needed
    /// to run a module
    pub(crate) runtime: Option<GuestRuntime>,

    /// Keep a reference to the host functions
    _functions: Vec<Function>,

    /// Communication with the timer thread
    pub(crate) timer_tx: std::sync::mpsc::Sender<TimerAction>,

    /// Information that gets populated after a call
    pub(crate) output: Output,

    /// Set to `true` when de-initializarion may have occured (i.e.a call to `_start`),
    /// in this case we need to re-initialize the entire module.
    pub(crate) store_needs_reset: bool,

    pub(crate) debug_options: DebugOptions,
}

unsafe impl Send for Plugin {}
unsafe impl Sync for Plugin {}

impl std::fmt::Debug for Plugin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Plugin({})", self.id)
    }
}

impl Internal for Plugin {
    fn store(&self) -> &Store<CurrentPlugin> {
        &self.store
    }

    fn store_mut(&mut self) -> &mut Store<CurrentPlugin> {
        &mut self.store
    }

    fn linker(&self) -> &Linker<CurrentPlugin> {
        &self.linker
    }

    fn linker_mut(&mut self) -> &mut Linker<CurrentPlugin> {
        &mut self.linker
    }

    fn linker_and_store(&mut self) -> (&mut Linker<CurrentPlugin>, &mut Store<CurrentPlugin>) {
        (&mut self.linker, &mut self.store)
    }
}

pub(crate) fn profiling_strategy() -> ProfilingStrategy {
    match std::env::var("EXTISM_PROFILE").as_deref() {
        Ok("perf") => ProfilingStrategy::PerfMap,
        Ok("jitdump") => ProfilingStrategy::JitDump,
        Ok("vtune") => ProfilingStrategy::VTune,
        Ok(x) => {
            warn!("Invalid value for EXTISM_PROFILE: {x}");
            ProfilingStrategy::None
        }
        Err(_) => ProfilingStrategy::None,
    }
}

/// Defines an input type for Wasm data.
///
/// Types that implement `Into<WasmInput>` can be passed directly into `Plugin::new`
#[derive(Clone)]
pub enum WasmInput<'a> {
    /// Raw Wasm module
    Data(std::borrow::Cow<'a, [u8]>),
    /// Owned manifest
    Manifest(Manifest),
    /// Borrowed manifest
    ManifestRef(&'a Manifest),
}

impl<'a> From<Manifest> for WasmInput<'a> {
    fn from(value: Manifest) -> Self {
        WasmInput::Manifest(value)
    }
}

impl<'a> From<&'a Manifest> for WasmInput<'a> {
    fn from(value: &'a Manifest) -> Self {
        WasmInput::ManifestRef(value)
    }
}

impl<'a> From<&'a mut Manifest> for WasmInput<'a> {
    fn from(value: &'a mut Manifest) -> Self {
        WasmInput::ManifestRef(value)
    }
}

impl<'a> From<&'a [u8]> for WasmInput<'a> {
    fn from(value: &'a [u8]) -> Self {
        WasmInput::Data(value.into())
    }
}

impl<'a> From<&'a str> for WasmInput<'a> {
    fn from(value: &'a str) -> Self {
        WasmInput::Data(value.as_bytes().into())
    }
}

impl<'a> From<Vec<u8>> for WasmInput<'a> {
    fn from(value: Vec<u8>) -> Self {
        WasmInput::Data(value.into())
    }
}

impl<'a> From<&'a Vec<u8>> for WasmInput<'a> {
    fn from(value: &'a Vec<u8>) -> Self {
        WasmInput::Data(value.into())
    }
}

fn add_module<T: 'static>(
    store: &mut Store<T>,
    linker: &mut Linker<T>,
    linked: &mut BTreeSet<String>,
    modules: &BTreeMap<String, Module>,
    name: String,
    module: &Module,
) -> Result<(), Error> {
    if linked.contains(&name) {
        return Ok(());
    }

    for import in module.imports() {
        if !linked.contains(import.module()) {
            if let Some(m) = modules.get(import.module()) {
                add_module(
                    store,
                    linker,
                    linked,
                    modules,
                    import.module().to_string(),
                    m,
                )?;
            }
        }
    }

    linker.module(store, name.as_str(), module)?;
    linked.insert(name);

    Ok(())
}

impl Plugin {
    /// Create a new plugin from a Manifest or WebAssembly module, and host functions. The `with_wasi`
    /// parameter determines whether or not the module should be executed with WASI enabled.
    pub fn new<'a>(
        wasm: impl Into<WasmInput<'a>>,
        imports: impl IntoIterator<Item = Function>,
        with_wasi: bool,
    ) -> Result<Plugin, Error> {
        Self::build_new(wasm.into(), imports, with_wasi, Default::default(), None)
    }

    pub(crate) fn build_new(
        wasm: WasmInput<'_>,
        imports: impl IntoIterator<Item = Function>,
        with_wasi: bool,
        debug_options: DebugOptions,
        cache_dir: Option<Option<PathBuf>>,
    ) -> Result<Plugin, Error> {
        // Setup wasmtime types
        let mut config = Config::new();
        config
            .epoch_interruption(true)
            .debug_info(debug_options.debug_info)
            .coredump_on_trap(debug_options.coredump.is_some())
            .profiler(debug_options.profiling_strategy)
            .wasm_tail_call(true)
            .wasm_function_references(true);

        match cache_dir {
            Some(None) => (),
            Some(Some(path)) => {
                config.cache_config_load(path)?;
            }
            None => {
                if let Ok(env) = std::env::var("EXTISM_CACHE_CONFIG") {
                    if !env.is_empty() {
                        config.cache_config_load(&env)?;
                    }
                } else {
                    config.cache_config_load_default()?;
                }
            }
        }

        let engine = Engine::new(&config)?;
        let (manifest, modules) = manifest::load(&engine, wasm)?;
        if modules.len() <= 1 {
            anyhow::bail!("No wasm modules provided");
        } else if !modules.contains_key(MAIN_KEY) {
            anyhow::bail!("No main module provided");
        }

        let available_pages = manifest.memory.max_pages;
        debug!("Available pages: {available_pages:?}");

        let id = uuid::Uuid::new_v4();
        let mut store = Store::new(
            &engine,
            CurrentPlugin::new(manifest, with_wasi, available_pages, id)?,
        );
        store.set_epoch_deadline(1);

        let mut linker = Linker::new(&engine);
        let mut imports: Vec<_> = imports.into_iter().collect();
        // Define PDK functions
        macro_rules! add_funcs {
            ($($name:ident($($args:expr),*) $(-> $($r:expr),*)?);* $(;)?) => {
                $(
                    let t = FuncType::new([$($args),*], [$($($r),*)?]);
                    linker.func_new(EXTISM_ENV_MODULE, stringify!($name), t, pdk::$name)?;
                )*
            };
        }

        // Add builtins
        use wasmtime::ValType::*;
        add_funcs!(
            config_get(I64) -> I64;
            var_get(I64) -> I64;
            var_set(I64, I64);
            http_request(I64, I64) -> I64;
            http_status_code() -> I32;
            log_warn(I64);
            log_info(I64);
            log_debug(I64);
            log_error(I64);
        );

        let mut linked = BTreeSet::new();
        linker.module(&mut store, EXTISM_ENV_MODULE, &modules[EXTISM_ENV_MODULE])?;
        linked.insert(EXTISM_ENV_MODULE.to_string());

        // If wasi is enabled then add it to the linker
        if with_wasi {
            wasmtime_wasi::add_to_linker(&mut linker, |x: &mut CurrentPlugin| {
                &mut x.wasi.as_mut().unwrap().ctx
            })?;
        }

        for f in &mut imports {
            let name = f.name();
            let ns = f.namespace().unwrap_or(EXTISM_USER_MODULE);
            unsafe {
                linker.func_new(ns, name, f.ty().clone(), &*(f.f.as_ref() as *const _))?;
            }
        }

        for (name, module) in modules.iter() {
            add_module(
                &mut store,
                &mut linker,
                &mut linked,
                &modules,
                name.clone(),
                module,
            )?;
        }

        let main = &modules[MAIN_KEY];
        let instance_pre = linker.instantiate_pre(main)?;
        let timer_tx = Timer::tx();
        let mut plugin = Plugin {
            modules,
            linker,
            instance: std::sync::Arc::new(std::sync::Mutex::new(None)),
            instance_pre,
            store,
            runtime: None,
            id,
            timer_tx: timer_tx.clone(),
            cancel_handle: CancelHandle { id, timer_tx },
            instantiations: 0,
            output: Output::default(),
            store_needs_reset: false,
            debug_options,
            _functions: imports,
        };

        plugin.current_plugin_mut().store = &mut plugin.store;
        plugin.current_plugin_mut().linker = &mut plugin.linker;
        if available_pages.is_some() {
            plugin
                .store
                .limiter(|internal| internal.memory_limiter.as_mut().unwrap());
        }
        debug!("{} created", plugin.id);
        Ok(plugin)
    }

    // Resets the store and linker to avoid running into Wasmtime memory limits
    pub(crate) fn reset_store(
        &mut self,
        instance_lock: &mut std::sync::MutexGuard<Option<Instance>>,
    ) -> Result<(), Error> {
        if self.store_needs_reset {
            let engine = self.store.engine().clone();
            let internal = self.current_plugin_mut();
            self.store = Store::new(
                &engine,
                CurrentPlugin::new(
                    internal.manifest.clone(),
                    internal.wasi.is_some(),
                    internal.available_pages,
                    self.id,
                )?,
            );
            self.store.set_epoch_deadline(1);
            let store = &mut self.store as *mut _;
            let linker = &mut self.linker as *mut _;
            let current_plugin = self.current_plugin_mut();
            current_plugin.store = store;
            current_plugin.linker = linker;
            if current_plugin.available_pages.is_some() {
                self.store
                    .limiter(|internal| internal.memory_limiter.as_mut().unwrap());
            }

            let main = &self.modules[MAIN_KEY];
            for (name, module) in self.modules.iter() {
                if name != MAIN_KEY {
                    self.linker.module(&mut self.store, name, module)?;
                }
            }
            self.instantiations = 0;
            self.instance_pre = self.linker.instantiate_pre(main)?;
            **instance_lock = None;
            self.store_needs_reset = false;
        }
        Ok(())
    }

    // Instantiate the module. This is done lazily to avoid running any code outside of the `call` function,
    // since wasmtime may execute a start function (if configured) at instantiation time,
    pub(crate) fn instantiate(
        &mut self,
        instance_lock: &mut std::sync::MutexGuard<Option<Instance>>,
    ) -> Result<(), Error> {
        if instance_lock.is_some() {
            return Ok(());
        }

        let instance = self.instance_pre.instantiate(&mut self.store)?;
        trace!(
            plugin = self.id.to_string(),
            "Plugin::instance is none, instantiating"
        );
        **instance_lock = Some(instance);
        self.instantiations += 1;
        if let Some(limiter) = &mut self.current_plugin_mut().memory_limiter {
            limiter.reset();
        }
        self.detect_guest_runtime(instance_lock);
        self.initialize_guest_runtime()?;
        Ok(())
    }

    /// Get an exported function by name
    pub(crate) fn get_func(
        &mut self,
        instance_lock: &mut std::sync::MutexGuard<Option<Instance>>,
        function: impl AsRef<str>,
    ) -> Option<Func> {
        if let Some(instance) = &mut **instance_lock {
            instance.get_func(&mut self.store, function.as_ref())
        } else {
            None
        }
    }

    /// Returns `true` if the given function exists, otherwise `false`
    pub fn function_exists(&mut self, function: impl AsRef<str>) -> bool {
        self.modules[MAIN_KEY]
            .get_export(function.as_ref())
            .map(|x| {
                if let Some(f) = x.func() {
                    let (params, mut results) = (f.params(), f.results());
                    match (params.len(), results.len()) {
                        (0, 1) => results.next() == Some(wasmtime::ValType::I32),
                        (0, 0) => true,
                        _ => false,
                    }
                } else {
                    false
                }
            })
            .unwrap_or(false)
    }

    // Store input in memory and re-initialize `Internal` pointer
    pub(crate) fn set_input(&mut self, input: *const u8, mut len: usize) -> Result<(), Error> {
        self.output = Output::default();
        self.clear_error()?;
        let id = self.id.to_string();

        if input.is_null() {
            len = 0;
        }

        {
            let store = &mut self.store as *mut _;
            let linker = &mut self.linker as *mut _;
            let current_plugin = self.current_plugin_mut();
            current_plugin.store = store;
            current_plugin.linker = linker;
        }

        let bytes = unsafe { std::slice::from_raw_parts(input, len) };
        debug!(plugin = &id, "input size: {}", bytes.len());

        self.reset()?;
        let handle = self.current_plugin_mut().memory_new(bytes)?;

        if let Some(f) = self
            .linker
            .get(&mut self.store, EXTISM_ENV_MODULE, "input_set")
        {
            f.into_func().unwrap().call(
                &mut self.store,
                &[Val::I64(handle.offset() as i64), Val::I64(len as i64)],
                &mut [],
            )?;
        }

        Ok(())
    }

    /// Reset Extism runtime, this will invalidate all allocated memory
    pub fn reset(&mut self) -> Result<(), Error> {
        let id = self.id.to_string();

        if let Some(f) = self.linker.get(&mut self.store, EXTISM_ENV_MODULE, "reset") {
            f.into_func().unwrap().call(&mut self.store, &[], &mut [])?;
        } else {
            error!(plugin = &id, "call to extism:host/env::reset failed");
        }

        Ok(())
    }

    /// Determine if wasi is enabled
    pub fn has_wasi(&self) -> bool {
        self.current_plugin().wasi.is_some()
    }

    // Do a best-effort attempt to detect any guest runtime.
    fn detect_guest_runtime(
        &mut self,
        instance_lock: &mut std::sync::MutexGuard<Option<Instance>>,
    ) {
        // Check for Haskell runtime initialization functions
        // Initialize Haskell runtime if `hs_init` is present,
        // by calling the `hs_init` export
        if let Some(init) = self.get_func(instance_lock, "hs_init") {
            let reactor_init = if let Some(init) = self.get_func(instance_lock, "_initialize") {
                if init.typed::<(), ()>(&self.store()).is_err() {
                    trace!(
                        plugin = self.id.to_string(),
                        "_initialize function found with type {:?}",
                        init.ty(self.store())
                    );
                    None
                } else {
                    trace!(plugin = self.id.to_string(), "WASI reactor module detected");
                    Some(init)
                }
            } else {
                None
            };
            self.runtime = Some(GuestRuntime::Haskell { init, reactor_init });
            return;
        }

        // Check for `__wasm_call_ctors` or `_initialize`, this is used by WASI to
        // initialize certain interfaces.
        let init = if let Some(init) = self.get_func(instance_lock, "__wasm_call_ctors") {
            if init.typed::<(), ()>(&self.store()).is_err() {
                trace!(
                    plugin = self.id.to_string(),
                    "__wasm_call_ctors function found with type {:?}",
                    init.ty(self.store())
                );
                return;
            }
            trace!(plugin = self.id.to_string(), "WASI runtime detected");
            init
        } else if let Some(init) = self.get_func(instance_lock, "_initialize") {
            if init.typed::<(), ()>(&self.store()).is_err() {
                trace!(
                    plugin = self.id.to_string(),
                    "_initialize function found with type {:?}",
                    init.ty(self.store())
                );
                return;
            }
            trace!(plugin = self.id.to_string(), "reactor module detected");
            init
        } else {
            return;
        };

        self.runtime = Some(GuestRuntime::Wasi { init });

        trace!(plugin = self.id.to_string(), "no runtime detected");
    }

    // Initialize the guest runtime
    pub(crate) fn initialize_guest_runtime(&mut self) -> Result<(), Error> {
        let mut store = &mut self.store;
        if let Some(runtime) = &self.runtime {
            trace!(plugin = self.id.to_string(), "Plugin::initialize_runtime");
            match runtime {
                GuestRuntime::Haskell { init, reactor_init } => {
                    if let Some(reactor_init) = reactor_init {
                        reactor_init.call(&mut store, &[], &mut [])?;
                    }
                    let mut results = vec![Val::null(); init.ty(&store).results().len()];
                    init.call(
                        &mut store,
                        &[Val::I32(0), Val::I32(0)],
                        results.as_mut_slice(),
                    )?;
                    debug!(
                        plugin = self.id.to_string(),
                        "initialized Haskell language runtime"
                    );
                }
                GuestRuntime::Wasi { init } => {
                    init.call(&mut store, &[], &mut [])?;
                    debug!(plugin = self.id.to_string(), "initialied WASI runtime");
                }
            }
        }

        Ok(())
    }

    // Return the position of the output in memory
    fn output_memory_position(&mut self) -> Result<(u64, u64), Error> {
        let out = &mut [Val::I64(0)];
        let out_len = &mut [Val::I64(0)];
        let mut store = &mut self.store;
        if let Some(f) = self
            .linker
            .get(&mut store, EXTISM_ENV_MODULE, "output_offset")
        {
            f.into_func().unwrap().call(&mut store, &[], out)?;
        } else {
            anyhow::bail!("unable to set output")
        }
        if let Some(f) = self
            .linker
            .get(&mut store, EXTISM_ENV_MODULE, "output_length")
        {
            f.into_func().unwrap().call(&mut store, &[], out_len)?;
        } else {
            anyhow::bail!("unable to set output length")
        }

        let offs = out[0].unwrap_i64() as u64;
        let len = out_len[0].unwrap_i64() as u64;
        Ok((offs, len))
    }

    // Get the output data after a call has returned
    fn output<'a, T: FromBytes<'a>>(&'a mut self) -> Result<T, Error> {
        let offs = self.output.offset;
        let len = self.output.length;
        T::from_bytes(
            self.current_plugin_mut()
                .memory_bytes(unsafe { MemoryHandle::new(offs, len) })?,
        )
    }

    // Cache output memory and error information after call is complete
    fn get_output_after_call(&mut self) -> Result<(), Error> {
        let (offs, len) = self.output_memory_position()?;
        self.output.offset = offs;
        self.output.length = len;
        debug!(
            plugin = self.id.to_string(),
            "output offset={}, length={}", offs, len
        );

        let (offs, len) = self.current_plugin_mut().get_error_position();
        self.output.error_offset = offs;
        self.output.error_length = len;
        debug!(
            plugin = self.id.to_string(),
            "error offset={}, length={}", offs, len
        );
        Ok(())
    }

    // Implements the build of the `call` function, `raw_call` is also used in the SDK
    // code
    pub(crate) fn raw_call(
        &mut self,
        lock: &mut std::sync::MutexGuard<Option<Instance>>,
        name: impl AsRef<str>,
        input: impl AsRef<[u8]>,
    ) -> Result<i32, (Error, i32)> {
        let name = name.as_ref();
        let input = input.as_ref();

        if let Err(e) = self.reset_store(lock) {
            error!(
                plugin = self.id.to_string(),
                "call to Plugin::reset_store failed: {e:?}"
            );
        }

        self.instantiate(lock).map_err(|e| (e, -1))?;

        self.set_input(input.as_ptr(), input.len())
            .map_err(|x| (x, -1))?;

        let func = match self.get_func(lock, name) {
            Some(x) => x,
            None => return Err((anyhow::anyhow!("Function not found: {name}"), -1)),
        };

        // Check the number of results, reject functions with more than 1 result
        let n_results = func.ty(self.store()).results().len();
        if n_results > 1 {
            return Err((
                anyhow::anyhow!("Function {name} has {n_results} results, expected 0 or 1"),
                -1,
            ));
        }

        // Start timer
        self.timer_tx
            .send(TimerAction::Start {
                id: self.id,
                engine: self.store.engine().clone(),
                duration: self
                    .current_plugin()
                    .manifest
                    .timeout_ms
                    .map(std::time::Duration::from_millis),
            })
            .expect("Timer should start");
        self.store.epoch_deadline_trap();
        self.store.set_epoch_deadline(1);

        // Call the function
        let mut results = vec![wasmtime::Val::null(); n_results];
        let mut res = func.call(self.store_mut(), &[], results.as_mut_slice());

        // Stop timer
        self.store
            .epoch_deadline_callback(|_| Ok(UpdateDeadline::Continue(1)));
        let _ = self.timer_tx.send(TimerAction::Stop { id: self.id });
        self.store_needs_reset = name == "_start";

        // Get extism error
        self.get_output_after_call().map_err(|x| (x, -1))?;
        let mut rc = 0;
        if !results.is_empty() {
            rc = results[0].i32().unwrap_or(-1);
            debug!(plugin = self.id.to_string(), "got return code: {}", rc);
        }

        if self.output.error_offset != 0 && self.output.error_length != 0 {
            let handle = MemoryHandle {
                offset: self.output.error_offset,
                length: self.output.error_length,
            };
            if let Ok(e) = self.current_plugin_mut().memory_str(handle) {
                let x = e.to_string();
                error!(
                    plugin = self.id.to_string(),
                    "call to {name} returned with error message: {}", x
                );
                if let Err(e) = res {
                    res = Err(Error::msg(x).context(e));
                } else {
                    res = Err(Error::msg(x))
                }
            } else {
                res = Err(Error::msg(format!(
                    "Call to Extism plugin function {name} encountered an error"
                )));
            }
        }

        match res {
            Ok(()) => Ok(rc),
            Err(e) => {
                if let Some(coredump) = e.downcast_ref::<wasmtime::WasmCoreDump>() {
                    if let Some(file) = self.debug_options.coredump.clone() {
                        debug!(
                            plugin = self.id.to_string(),
                            "saving coredump to {}",
                            file.display()
                        );

                        if let Err(e) =
                            std::fs::write(file, coredump.serialize(self.store_mut(), "extism"))
                        {
                            error!(
                                plugin = self.id.to_string(),
                                "unable to write coredump: {:?}", e
                            );
                        }
                    }
                }

                if let Some(file) = &self.debug_options.memdump.clone() {
                    trace!(plugin = self.id.to_string(), "memory dump enabled");
                    if let Some(memory) = self.current_plugin_mut().memory() {
                        debug!(
                            plugin = self.id.to_string(),
                            "dumping memory to {}",
                            file.display()
                        );
                        let data = memory.data(&mut self.store);
                        if let Err(e) = std::fs::write(file, data) {
                            error!(
                                plugin = self.id.to_string(),
                                "unable to write memory dump: {:?}", e
                            );
                        }
                    } else {
                        error!(
                            plugin = self.id.to_string(),
                            "unable to get extism memory for writing to disk",
                        );
                    }
                }

                let wasi_exit_code = e
                    .downcast_ref::<wasmtime_wasi::I32Exit>()
                    .map(|e| e.0)
                    .or_else(|| {
                        e.downcast_ref::<wasmtime_wasi::preview2::I32Exit>()
                            .map(|e| e.0)
                    });
                if let Some(exit_code) = wasi_exit_code {
                    debug!(
                        plugin = self.id.to_string(),
                        "WASI exit code: {}", exit_code
                    );
                    if exit_code == 0 {
                        return Ok(0);
                    }

                    return Err((e.context("WASI exit code"), exit_code));
                }

                // Handle timeout interrupts
                if let Some(wasmtime::Trap::Interrupt) = e.downcast_ref::<wasmtime::Trap>() {
                    debug!(plugin = self.id.to_string(), "call to {name} timed out");
                    return Err((Error::msg("timeout"), rc));
                }

                // Handle out-of-memory error from `MemoryLimiter`
                let cause = e.root_cause().to_string();
                if cause == "oom" {
                    debug!(
                        plugin = self.id.to_string(),
                        "call to {name} ran out of memory"
                    );
                    return Err((Error::msg(cause), rc));
                }

                error!(
                    plugin = self.id.to_string(),
                    "call to {name} encountered an error: {e:?}"
                );
                Err((e, rc))
            }
        }
    }

    /// Call a function by name with the given input, the return value is
    /// the output data returned by the plugin. The return type can be anything that implements
    /// [FromBytes]. This data will be invalidated next time the plugin is called.
    ///
    /// # Arguments
    ///
    /// * `name` - A string representing the name of the export function to call
    /// * `input` - The input argument to the function. Type should implment [ToBytes].
    ///
    /// # Examples
    ///
    /// ```ignore
    /// // call takes a ToBytes and FromBytes type
    /// // this function takes an &str and returns an &str
    /// let output = plugin.call::<&str, &str>("greet", "Benjamin")?;
    /// assert_eq!(output, "Hello, Benjamin!");
    /// ```
    pub fn call<'a, 'b, T: ToBytes<'a>, U: FromBytes<'b>>(
        &'b mut self,
        name: impl AsRef<str>,
        input: T,
    ) -> Result<U, Error> {
        let lock = self.instance.clone();
        let mut lock = lock.lock().unwrap();
        let data = input.to_bytes()?;
        self.raw_call(&mut lock, name, data)
            .map_err(|e| e.0)
            .and_then(move |_| self.output())
    }

    /// Similar to `Plugin::call`, but returns the Extism error code along with the
    /// `Error`. It is assumed if `Ok(_)` is returned that the error code was `0`.
    ///
    /// All Extism plugin calls return an error code, `Plugin::call` consumes the error code,
    /// while `Plugin::call_get_error_code` preserves it - this function should only be used
    /// when you need to inspect the actual return value of a plugin function when it fails.
    pub fn call_get_error_code<'a, 'b, T: ToBytes<'a>, U: FromBytes<'b>>(
        &'b mut self,
        name: impl AsRef<str>,
        input: T,
    ) -> Result<U, (Error, i32)> {
        let lock = self.instance.clone();
        let mut lock = lock.lock().unwrap();
        let data = input.to_bytes().map_err(|e| (e, -1))?;
        self.raw_call(&mut lock, name, data)
            .and_then(move |_| self.output().map_err(|e| (e, -1)))
    }

    /// Get a `CancelHandle`, which can be used from another thread to cancel a running plugin
    pub fn cancel_handle(&self) -> CancelHandle {
        self.cancel_handle.clone()
    }

    pub(crate) fn clear_error(&mut self) -> Result<(), Error> {
        trace!(plugin = self.id.to_string(), "clearing error");
        let (linker, mut store) = self.linker_and_store();
        if let Some(f) = linker.get(&mut store, EXTISM_ENV_MODULE, "error_set") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(0)], &mut [])?;
            Ok(())
        } else {
            anyhow::bail!("Plugin::clear_error failed, extism:host/env::error_set not found")
        }
    }

    // A convenience method to set the plugin error and return a value
    pub(crate) fn return_error<E>(
        &mut self,
        instance_lock: &mut std::sync::MutexGuard<Option<Instance>>,
        e: impl std::fmt::Display,
        x: E,
    ) -> E {
        if instance_lock.is_none() {
            error!(
                plugin = self.id.to_string(),
                "no instance, unable to set error: {}", e
            );
            return x;
        }
        match self.current_plugin_mut().set_error(e.to_string()) {
            Ok((a, b)) => {
                self.output.error_offset = a;
                self.output.error_length = b;
            }
            Err(e) => {
                error!(plugin = self.id.to_string(), "unable to set error: {e:?}")
            }
        }
        x
    }
}

// Enumerates the PDK languages that need some additional initialization
#[derive(Clone)]
pub(crate) enum GuestRuntime {
    Haskell {
        init: Func,
        reactor_init: Option<Func>,
    },
    Wasi {
        init: Func,
    },
}

/// The `typed_plugin` macro is used to create a newtype wrapper around `Plugin` with methods defined for the specified functions.
///
/// For example, we can define a new type `MyPlugin` that automatically implements `From`/`Into` for `Plugin`
/// ```rust
/// #[derive(serde::Deserialize)]
/// struct Count {
///   count: usize,
/// }
///
/// extism::typed_plugin!(MyPlugin {
///   count_vowels(&str) -> extism::convert::Json<Count>;
/// });
///
/// # const WASM: &[u8] = include_bytes!("../../wasm/code.wasm");
/// // Convert from `Plugin` to `MyPlugin`
/// let mut plugin: MyPlugin = extism::Plugin::new(WASM, [], true).unwrap().try_into().unwrap();
/// // and call the `count_vowels` function
/// let count = plugin.count_vowels("this is a test").unwrap();
/// ```
#[macro_export]
macro_rules! typed_plugin {
    ($pub:vis $name:ident {$($f:ident $(< $( $lt:tt $( : $clt:path )? ),+ >)? ($input:ty) -> $output:ty);*$(;)?}) => {
        $pub struct $name(pub $crate::Plugin);

        unsafe impl Send for $name {}
        unsafe impl Sync for $name {}

        impl TryFrom<$crate::Plugin> for $name {
            type Error = $crate::Error;
            fn try_from(mut x: $crate::Plugin) -> Result<Self, Self::Error> {
                $(
                    if !x.function_exists(stringify!($f)) {
                        return Err($crate::Error::msg(format!("Invalid function: {}", stringify!($f))));
                    }
                )*
                Ok($name(x))
            }
        }

        impl From<$name> for $crate::Plugin {
            fn from(x: $name) -> Self {
                x.0
            }
        }

        impl $name {
            $(
                pub fn $f<'a, $( $( $lt $( : $clt )? ),+ )? >(&'a mut self, input: $input) -> Result<$output, $crate::Error> {
                    self.0.call(stringify!($f), input)
                }
            )*
        }
    };
}
