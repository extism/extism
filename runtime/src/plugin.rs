use std::collections::BTreeMap;

use crate::*;

#[derive(Default)]
pub(crate) struct Output {
    pub(crate) offset: u64,
    pub(crate) length: u64,
    pub(crate) error_offset: u64,
    pub(crate) error_length: u64,
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
    pub(crate) cancel_handle: sdk::ExtismCancelHandle,

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
    pub(crate) runtime: Option<Runtime>,

    /// Keep a reference to the host functions
    _functions: Vec<Function>,

    /// Communication with the timer thread
    pub(crate) timer_tx: std::sync::mpsc::Sender<TimerAction>,

    /// Information that gets populated after a call
    pub(crate) output: Output,

    /// Set to `true` when de-initializarion may have occured (i.e.a call to `_start`),
    /// in this case we need to re-initialize the entire module.
    pub(crate) needs_reset: bool,
}

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

const EXPORT_MODULE_NAME: &str = "env";

fn profiling_strategy() -> ProfilingStrategy {
    match std::env::var("EXTISM_PROFILE").as_deref() {
        Ok("perf") => ProfilingStrategy::PerfMap,
        Ok(x) => {
            log::warn!("Invalid value for EXTISM_PROFILE: {x}");
            ProfilingStrategy::None
        }
        Err(_) => ProfilingStrategy::None,
    }
}

fn calculate_available_memory(
    available_pages: &mut Option<u32>,
    modules: &BTreeMap<String, Module>,
) -> anyhow::Result<()> {
    let available_pages = match available_pages {
        Some(p) => p,
        None => return Ok(()),
    };

    let max_pages = *available_pages;
    let mut fail_memory_check = false;
    let mut total_memory_needed = 0;
    for (name, module) in modules.iter() {
        if name == "env" {
            continue;
        }
        let mut memories = 0;
        for export in module.exports() {
            if let Some(memory) = export.ty().memory() {
                memories += 1;
                let memory_max = memory.maximum();
                match memory_max {
                    None => anyhow::bail!("Unbounded memory in module {name}, when `memory.max_pages` is set in the manifest all modules \
                                           must have a maximum bound set on an exported memory"),
                    Some(m) => {
                        total_memory_needed += m;
                        if !fail_memory_check {
                            continue;
                        }

                        *available_pages = available_pages.saturating_sub(m as u32);
                        if *available_pages == 0 {
                            fail_memory_check = true;
                        }
                    }
                }
            }
        }

        if memories == 0 {
            anyhow::bail!("No memory exported from module {name}, when `memory.max_pages` is set in the manifest all modules must \
                           have a maximum bound set on an exported memory");
        }
    }

    if fail_memory_check {
        anyhow::bail!("Not enough memory configured to run the provided plugin, `memory.max_pages` is set to {max_pages} in the manifest \
                       but {total_memory_needed} pages are needed by the plugin");
    }

    Ok(())
}

fn deadline_callback(_: StoreContextMut<CurrentPlugin>) -> Result<UpdateDeadline, Error> {
    Err(Error::msg("timeout"))
}

impl Plugin {
    /// Create a new plugin from the given manifest
    pub fn new_with_manifest(
        manifest: &Manifest,
        functions: impl IntoIterator<Item = Function>,
        with_wasi: bool,
    ) -> Result<Plugin, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::new(data, functions, with_wasi)
    }

    /// Create a new plugin from the given WASM code
    pub fn new<'a>(
        wasm: impl AsRef<[u8]>,
        imports: impl IntoIterator<Item = Function>,
        with_wasi: bool,
    ) -> Result<Plugin, Error> {
        // Create a new engine, if the `EXITSM_DEBUG` environment variable is set
        // then we enable debug info
        let engine = Engine::new(
            Config::new()
                .epoch_interruption(true)
                .debug_info(std::env::var("EXTISM_DEBUG").is_ok())
                .profiler(profiling_strategy()),
        )?;
        let mut imports = imports.into_iter();
        let (manifest, modules) = manifest::load(&engine, wasm.as_ref())?;

        // Calculate how much memory is available based on the value of `max_pages` and the exported
        // memory of the modules. An error will be returned if a module doesn't have an exported memory
        // or there is no maximum set for a module's exported memory.
        let mut available_pages = manifest.memory.max_pages;
        calculate_available_memory(&mut available_pages, &modules)?;
        log::trace!("Available pages: {available_pages:?}");

        let mut store = Store::new(
            &engine,
            CurrentPlugin::new(manifest, with_wasi, available_pages)?,
        );

        store.set_epoch_deadline(1);

        if available_pages.is_some() {
            store.limiter(|internal| internal.memory_limiter.as_mut().unwrap());
        }
        let mut linker = Linker::new(&engine);
        linker.allow_shadowing(true);

        // If wasi is enabled then add it to the linker
        if with_wasi {
            wasmtime_wasi::add_to_linker(&mut linker, |x: &mut CurrentPlugin| {
                &mut x.wasi.as_mut().unwrap().ctx
            })?;

            #[cfg(feature = "nn")]
            wasmtime_wasi_nn::add_to_linker(&mut linker, |x: &mut CurrentPlugin| {
                &mut x.wasi.as_mut().unwrap().nn
            })?;
        }
        // Get the `main` module, or the last one if `main` doesn't exist
        let (main_name, main) = modules.get("main").map(|x| ("main", x)).unwrap_or_else(|| {
            let entry = modules.iter().last().unwrap();
            (entry.0.as_str(), entry.1)
        });

        // Define PDK functions
        macro_rules! define_funcs {
            ($m:expr, { $($name:ident($($args:expr),*) $(-> $($r:expr),*)?);* $(;)?}) => {
                match $m {
                $(
                    concat!("extism_", stringify!($name)) => {
                        let t = FuncType::new([$($args),*], [$($($r),*)?]);
                        linker.func_new(EXPORT_MODULE_NAME, concat!("extism_", stringify!($name)), t, pdk::$name)?;
                        continue
                    }
                )*
                    _ => ()
                }
            };
        }

        // Add builtins
        for (name, module) in modules.iter() {
            if name != main_name {
                linker.module(&mut store, name, module)?;
            }
            for import in module.imports() {
                let module_name = import.module();
                let name = import.name();
                use wasmtime::ValType::*;

                if module_name == EXPORT_MODULE_NAME {
                    define_funcs!(name,  {
                        config_get(I64) -> I64;
                        var_get(I64) -> I64;
                        var_set(I64, I64);
                        http_request(I64, I64) -> I64;
                        http_status_code() -> I32;
                        log_warn(I64);
                        log_info(I64);
                        log_debug(I64);
                        log_error(I64);
                    });
                }
            }
        }

        for f in &mut imports {
            let name = f.name().to_string();
            let ns = f.namespace().unwrap_or(EXPORT_MODULE_NAME);
            linker.func_new(ns, &name, f.ty().clone(), unsafe {
                &*std::sync::Arc::as_ptr(&f.f)
            })?;
        }

        let instance_pre = linker.instantiate_pre(&main)?;
        let id = uuid::Uuid::new_v4();
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
            cancel_handle: sdk::ExtismCancelHandle {
                id: id.clone(),
                timer_tx,
            },
            instantiations: 0,
            output: Output::default(),
            _functions: imports.collect(),
            needs_reset: false,
        };

        plugin.current_plugin_mut().store = &mut plugin.store;
        plugin.current_plugin_mut().linker = &mut plugin.linker;
        Ok(plugin)
    }

    pub(crate) fn reset_store(
        &mut self,
        instance_lock: &mut std::sync::MutexGuard<Option<Instance>>,
    ) -> Result<(), Error> {
        if self.instantiations > 100 {
            let engine = self.store.engine().clone();
            let internal = self.current_plugin_mut();
            self.store = Store::new(
                &engine,
                CurrentPlugin::new(
                    internal.manifest.clone(),
                    internal.wasi.is_some(),
                    internal.available_pages,
                )?,
            );

            self.store.set_epoch_deadline(1);

            if self.current_plugin().available_pages.is_some() {
                self.store
                    .limiter(|internal| internal.memory_limiter.as_mut().unwrap());
            }

            let (main_name, main) = self
                .modules
                .get("main")
                .map(|x| ("main", x))
                .unwrap_or_else(|| {
                    let entry = self.modules.iter().last().unwrap();
                    (entry.0.as_str(), entry.1)
                });

            for (name, module) in self.modules.iter() {
                if name != main_name {
                    self.linker.module(&mut self.store, name, module)?;
                }
            }
            self.instantiations = 0;
            self.instance_pre = self.linker.instantiate_pre(&main)?;

            let store = &mut self.store as *mut _;
            let linker = &mut self.linker as *mut _;
            let current_plugin = self.current_plugin_mut();
            current_plugin.store = store;
            current_plugin.linker = linker;
        }

        **instance_lock = None;
        Ok(())
    }

    pub(crate) fn instantiate(
        &mut self,
        instance_lock: &mut std::sync::MutexGuard<Option<Instance>>,
    ) -> Result<(), Error> {
        if instance_lock.is_some() {
            return Ok(());
        }

        let instance = self.instance_pre.instantiate(&mut self.store)?;
        trace!("Plugin::instance is none, instantiating");
        **instance_lock = Some(instance);
        self.instantiations += 1;
        if let Some(limiter) = &mut self.current_plugin_mut().memory_limiter {
            limiter.reset();
        }
        self.detect_runtime(instance_lock);
        self.initialize_runtime()?;
        Ok(())
    }

    /// Get a function by name
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
        self.modules["main"]
            .get_export(function.as_ref())
            .map(|x| x.func().is_some())
            .unwrap_or(false)
    }

    /// Store input in memory and initialize `Internal` pointer
    pub(crate) fn set_input(&mut self, input: *const u8, mut len: usize) -> Result<(), Error> {
        self.output = Output::default();
        self.clear_error();

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
        trace!("Input size: {}", bytes.len());

        if let Some(f) = self.linker.get(&mut self.store, "env", "extism_reset") {
            f.into_func().unwrap().call(&mut self.store, &[], &mut [])?;
        } else {
            error!("Call to extism_reset failed");
        }

        let offs = self.current_plugin_mut().memory_alloc_bytes(bytes)?;

        if let Some(f) = self.linker.get(&mut self.store, "env", "extism_input_set") {
            f.into_func().unwrap().call(
                &mut self.store,
                &[Val::I64(offs as i64), Val::I64(len as i64)],
                &mut [],
            )?;
        }

        Ok(())
    }

    /// Determine if wasi is enabled
    pub fn has_wasi(&self) -> bool {
        self.current_plugin().wasi.is_some()
    }

    fn detect_runtime(&mut self, instance_lock: &mut std::sync::MutexGuard<Option<Instance>>) {
        // Check for Haskell runtime initialization functions
        // Initialize Haskell runtime if `hs_init` is present,
        // by calling the `hs_init` export
        if let Some(init) = self.get_func(instance_lock, "hs_init") {
            let reactor_init = if let Some(init) = self.get_func(instance_lock, "_initialize") {
                if init.typed::<(), ()>(&self.store()).is_err() {
                    trace!(
                        "_initialize function found with type {:?}",
                        init.ty(self.store())
                    );
                    None
                } else {
                    trace!("WASI reactor module detected");
                    Some(init)
                }
            } else {
                None
            };
            self.runtime = Some(Runtime::Haskell { init, reactor_init });
            return;
        }

        // Check for `__wasm_call_ctors` or `_initialize`, this is used by WASI to
        // initialize certain interfaces.
        let init = if let Some(init) = self.get_func(instance_lock, "__wasm_call_ctors") {
            if init.typed::<(), ()>(&self.store()).is_err() {
                trace!(
                    "__wasm_call_ctors function found with type {:?}",
                    init.ty(self.store())
                );
                return;
            }
            trace!("WASI runtime detected");
            init
        } else if let Some(init) = self.get_func(instance_lock, "_initialize") {
            if init.typed::<(), ()>(&self.store()).is_err() {
                trace!(
                    "_initialize function found with type {:?}",
                    init.ty(self.store())
                );
                return;
            }
            trace!("Reactor module detected");
            init
        } else {
            return;
        };

        self.runtime = Some(Runtime::Wasi { init });

        trace!("No runtime detected");
    }

    pub(crate) fn initialize_runtime(&mut self) -> Result<(), Error> {
        let mut store = &mut self.store;
        if let Some(runtime) = &self.runtime {
            trace!("Plugin::initialize_runtime");
            match runtime {
                Runtime::Haskell { init, reactor_init } => {
                    if let Some(reactor_init) = reactor_init {
                        reactor_init.call(&mut store, &[], &mut [])?;
                    }
                    let mut results = vec![Val::null(); init.ty(&store).results().len()];
                    init.call(
                        &mut store,
                        &[Val::I32(0), Val::I32(0)],
                        results.as_mut_slice(),
                    )?;
                    debug!("Initialized Haskell language runtime");
                }
                Runtime::Wasi { init } => {
                    init.call(&mut store, &[], &mut [])?;
                    debug!("Initialied WASI runtime");
                }
            }
        }

        Ok(())
    }

    fn output_memory_position(&mut self) -> (u64, u64) {
        let out = &mut [Val::I64(0)];
        let out_len = &mut [Val::I64(0)];
        let mut store = &mut self.store;
        self.linker
            .get(&mut store, "env", "extism_output_offset")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[], out)
            .unwrap();
        self.linker
            .get(&mut store, "env", "extism_output_length")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[], out_len)
            .unwrap();

        let offs = out[0].unwrap_i64() as u64;
        let len = out_len[0].unwrap_i64() as u64;
        (offs, len)
    }

    fn output(&mut self) -> &[u8] {
        trace!("Output offset: {}", self.output.offset);
        let offs = self.output.offset;
        let len = self.output.length;
        self.current_plugin_mut().memory_read(offs, len)
    }

    fn get_output_after_call(&mut self) {
        let (offs, len) = self.output_memory_position();
        self.output.offset = offs;
        self.output.length = len;

        let err = self.current_plugin_mut().get_error_position();
        self.output.error_offset = err.0;
        self.output.error_length = err.1;
    }

    pub(crate) fn raw_call(
        &mut self,
        lock: &mut std::sync::MutexGuard<Option<Instance>>,
        name: impl AsRef<str>,
        input: impl AsRef<[u8]>,
    ) -> Result<i32, (Error, i32)> {
        let name = name.as_ref();
        let input = input.as_ref();

        if self.needs_reset {
            if let Err(e) = self.reset_store(lock) {
                error!("Call to Plugin::reset_store failed: {e:?}");
            }
            self.needs_reset = false;
        }

        self.instantiate(lock).map_err(|e| (e, -1))?;

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
        self.set_input(input.as_ptr(), input.len())
            .map_err(|x| (x, -1))?;

        // Start timer
        self.timer_tx
            .send(TimerAction::Start {
                id: self.id.clone(),
                engine: self.store.engine().clone(),
                duration: self
                    .current_plugin()
                    .manifest
                    .timeout_ms
                    .map(std::time::Duration::from_millis),
            })
            .unwrap();
        self.store.epoch_deadline_callback(deadline_callback);

        // Call the function
        let mut results = vec![wasmtime::Val::null(); n_results];
        let res = func.call(self.store_mut(), &[], results.as_mut_slice());

        // Stop timer
        self.timer_tx
            .send(TimerAction::Stop {
                id: self.id.clone(),
            })
            .unwrap();
        self.store
            .epoch_deadline_callback(|_| Ok(UpdateDeadline::Continue(1)));

        self.get_output_after_call();

        match res {
            Ok(()) => {
                self.needs_reset = name == "_start";
            }
            Err(e) => match e.downcast::<wasmtime_wasi::I32Exit>() {
                Ok(exit) => {
                    trace!("WASI return code: {}", exit.0);
                    if exit.0 != 0 {
                        return Err((Error::msg("WASI return code"), exit.0));
                    }
                    return Ok(0);
                }
                Err(e) => {
                    if e.root_cause().to_string() == "timeout" {
                        return Err((Error::msg("timeout"), -1));
                    }

                    error!("Call: {e:?}");
                    return Err((e.context("Call failed"), -1));
                }
            },
        };

        // If `results` is empty and the return value wasn't a WASI exit code then
        // the call succeeded
        if results.is_empty() {
            return Ok(0);
        }

        // Return result to caller
        Ok(0)
    }

    /// Call a function by name with the given input, the return value is the output data returned by the plugin.
    /// This data will be invalidated next time the plugin is called.
    pub fn call(&mut self, name: impl AsRef<str>, input: impl AsRef<[u8]>) -> Result<&[u8], Error> {
        let lock = self.instance.clone();
        let mut lock = lock.lock().unwrap();
        self.raw_call(&mut lock, name, input)
            .map(|_| self.output())
            .map_err(|e| e.0)
    }

    /// Get a `CancelHandle`, which can be used from another thread to cancel a running plugin
    pub fn cancel_handle(&self) -> CancelHandle {
        self.cancel_handle.clone()
    }

    pub(crate) fn clear_error(&mut self) {
        let (linker, mut store) = self.linker_and_store();
        if let Some(f) = linker.get(&mut store, "env", "extism_error_set") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(0)], &mut [])
                .unwrap();
        }
    }
}

// Enumerates the supported PDK language runtimes
#[derive(Clone)]
pub(crate) enum Runtime {
    Haskell {
        init: Func,
        reactor_init: Option<Func>,
    },
    Wasi {
        init: Func,
    },
}
