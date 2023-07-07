use std::collections::BTreeMap;

use crate::*;

use pretty_hex::PrettyHex;

/// Plugin contains everything needed to execute a WASM function
pub struct Plugin {
    /// All modules that were provided to the linker
    pub modules: BTreeMap<String, Module>,

    /// Used to define functions and create new instances
    pub linker: Linker<Internal>,

    /// Instance provides the ability to call functions in a module
    pub instance: Instance,

    /// Keep track of the number of times we're instantiated, this exists
    /// to avoid issues with memory piling up since `Instance`s are only
    /// actually cleaned up along with a `Store`
    pub instantiations: usize,

    /// Handles interactions with WASM memory
    // pub memory: std::cell::UnsafeCell<PluginMemory>,
    pub store: Store<Internal>,

    /// The ID used to identify this plugin with the `Timer`
    pub timer_id: uuid::Uuid,

    /// A handle used to cancel execution of a plugin
    pub(crate) cancel_handle: sdk::ExtismCancelHandle,

    /// Runtime determines any initialization and cleanup functions needed
    /// to run a module
    pub(crate) runtime: Option<Runtime>,
}

impl InternalExt for Plugin {
    fn store(&self) -> &Store<Internal> {
        &self.store
    }

    fn store_mut(&mut self) -> &mut Store<Internal> {
        &mut self.store
    }

    fn linker(&self) -> &Linker<Internal> {
        &self.linker
    }

    fn linker_mut(&mut self) -> &mut Linker<Internal> {
        &mut self.linker
    }

    fn linker_and_store(&mut self) -> (&mut Linker<Internal>, &mut Store<Internal>) {
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
                memories += 1;
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

impl Plugin {
    /// Create a new plugin from the given WASM code
    pub fn new<'a>(
        wasm: impl AsRef<[u8]>,
        imports: impl IntoIterator<Item = &'a Function>,
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
        let (manifest, modules) = Manifest::new(&engine, wasm.as_ref())?;

        // Calculate how much memory is available based on the value of `max_pages` and the exported
        // memory of the modules. An error will be returned if a module doesn't have an exported memory
        // or there is no maximum set for a module's exported memory.
        let mut available_pages = manifest.as_ref().memory.max_pages;
        calculate_available_memory(&mut available_pages, &modules)?;
        log::trace!("Available pages: {available_pages:?}");

        let mut store = Store::new(
            &engine,
            Internal::new(manifest, with_wasi, available_pages)?,
        );
        store.epoch_deadline_callback(|_internal| Err(Error::msg("timeout")));

        let mut linker = Linker::new(&engine);
        linker.allow_shadowing(true);

        // If wasi is enabled then add it to the linker
        if with_wasi {
            wasmtime_wasi::add_to_linker(&mut linker, |x: &mut Internal| {
                &mut x.wasi.as_mut().unwrap().ctx
            })?;

            #[cfg(feature = "nn")]
            wasmtime_wasi_nn::add_to_linker(&mut linker, |x: &mut Internal| {
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

        for (name, module) in modules.iter() {
            if name != main_name {
                linker.module(&mut store, name, module)?;
            }
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

                    for f in &mut imports {
                        let name = f.name().to_string();
                        let ns = f.namespace().unwrap_or(EXPORT_MODULE_NAME);
                        linker.func_new(ns, &name, f.ty().clone(), unsafe {
                            &*std::sync::Arc::as_ptr(&f.f)
                        })?;
                    }
                }
            }
        }

        let instance = linker.instantiate(&mut store, main)?;
        let timer_id = uuid::Uuid::new_v4();
        let mut plugin = Plugin {
            modules,
            linker,
            instance,
            store,
            instantiations: 1,
            runtime: None,
            timer_id,
            cancel_handle: sdk::ExtismCancelHandle {
                id: timer_id,
                epoch_timer_tx: None,
            },
        };

        plugin.internal_mut().store = &mut plugin.store;
        plugin.internal_mut().linker = &mut plugin.linker;

        // Then detect runtime before returning the new plugin
        plugin.detect_runtime();
        Ok(plugin)
    }

    /// Get a function by name
    pub fn get_func(&mut self, function: impl AsRef<str>) -> Option<Func> {
        self.instance.get_func(&mut self.store, function.as_ref())
    }

    /// Store input in memory and initialize `Internal` pointer
    pub(crate) fn set_input(
        &mut self,
        input: *const u8,
        mut len: usize,
        tx: std::sync::mpsc::SyncSender<TimerAction>,
    ) {
        if input.is_null() {
            len = 0;
        }

        self.start_timer(&tx).unwrap();
        self.linker
            .get(&mut self.store, "env", "extism_reset")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut self.store, &[], &mut [])
            .unwrap();

        let offs = self.memory_alloc(len as u64);
        unsafe {
            self.memory()
                .as_mut_ptr()
                .add(offs as usize)
                .copy_from(input, len);
        }

        self.linker
            .get(&mut self.store, "env", "extism_input_set")
            .unwrap()
            .into_func()
            .unwrap()
            .call(
                &mut self.store,
                &[Val::I64(offs as i64), Val::I64(len as i64)],
                &mut [],
            )
            .unwrap();

        self.internal_mut().store = &mut self.store;
        self.internal_mut().linker = &mut self.linker;
    }

    /// Dump memory using trace! logging
    pub fn dump_memory(&mut self) {
        let data = self.memory();

        // Reads the position directly from memory, skips the memory metadata section
        let position = &data[1..9];
        let position = u64::from_le_bytes(position.try_into().unwrap());
        trace!("{:?}", data[16..position as usize].hex_dump());
    }

    /// Create a new instance from the same modules
    pub fn reinstantiate(&mut self) -> Result<(), Error> {
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

        let instance = self.linker.instantiate(&mut self.store, main)?;
        self.instance = instance;
        self.detect_runtime();
        self.instantiations += 1;
        Ok(())
    }

    /// Determine if wasi is enabled
    pub fn has_wasi(&self) -> bool {
        self.internal().wasi.is_some()
    }

    fn detect_runtime(&mut self) {
        // Check for Haskell runtime initialization functions
        // Initialize Haskell runtime if `hs_init` and `hs_exit` are present,
        // by calling the `hs_init` export
        if let Some(init) = self.get_func("hs_init") {
            if let Some(cleanup) = self.get_func("hs_exit") {
                if init.typed::<(i32, i32), ()>(&self.store()).is_err() {
                    trace!(
                        "hs_init function found with type {:?}",
                        init.ty(self.store())
                    );
                }
                self.runtime = Some(Runtime::Haskell { init, cleanup });
            }
            return;
        }

        // Check for `__wasm__call_ctors` and `__wasm_call_dtors`, this is used by WASI to
        // initialize certain interfaces.
        if self.has_wasi() {
            if let Some(init) = self.get_func("__wasm_call_ctors") {
                if init.typed::<(), ()>(&self.store()).is_err() {
                    trace!(
                        "__wasm_call_ctors function found with type {:?}",
                        init.ty(self.store())
                    );
                    return;
                }
                trace!("WASI runtime detected");
                if let Some(cleanup) = self.get_func("__wasm_call_dtors") {
                    if cleanup.typed::<(), ()>(&self.store()).is_err() {
                        trace!(
                            "__wasm_call_dtors function found with type {:?}",
                            cleanup.ty(self.store())
                        );
                        return;
                    }
                    self.runtime = Some(Runtime::Wasi {
                        init,
                        cleanup: Some(cleanup),
                    });
                    return;
                }

                self.runtime = Some(Runtime::Wasi {
                    init,
                    cleanup: None,
                });
            }
            return;
        }

        trace!("No runtime detected");
    }

    pub(crate) fn initialize_runtime(&mut self) -> Result<(), Error> {
        let mut store = &mut self.store;
        if let Some(runtime) = &self.runtime {
            trace!("Plugin::initialize_runtime");
            match runtime {
                Runtime::Haskell { init, cleanup: _ } => {
                    let mut results = vec![Val::null(); init.ty(&store).results().len()];
                    init.call(
                        &mut store,
                        &[Val::I32(0), Val::I32(0)],
                        results.as_mut_slice(),
                    )?;
                    debug!("Initialized Haskell language runtime");
                }
                Runtime::Wasi { init, cleanup: _ } => {
                    debug!("Calling __wasm_call_ctors");
                    init.call(&mut store, &[], &mut [])?;
                }
            }
        }

        Ok(())
    }

    #[inline(always)]
    pub(crate) fn cleanup_runtime(&mut self) -> Result<(), Error> {
        if let Some(runtime) = self.runtime.clone() {
            trace!("Plugin::cleanup_runtime");
            match runtime {
                Runtime::Wasi {
                    init: _,
                    cleanup: Some(cleanup),
                } => {
                    debug!("Calling __wasm_call_dtors");
                    cleanup.call(self.store_mut(), &[], &mut [])?;
                }
                Runtime::Wasi {
                    init: _,
                    cleanup: None,
                } => (),
                // Cleanup Haskell runtime if `hs_exit` and `hs_exit` are present,
                // by calling the `hs_exit` export
                Runtime::Haskell { init: _, cleanup } => {
                    let mut results = vec![Val::null(); cleanup.ty(self.store()).results().len()];
                    cleanup.call(self.store_mut(), &[], results.as_mut_slice())?;
                    debug!("Cleaned up Haskell language runtime");
                }
            }
        }

        Ok(())
    }

    /// Start the timer for a Plugin - this is used for both timeouts
    /// and cancellation
    pub(crate) fn start_timer(
        &mut self,
        tx: &std::sync::mpsc::SyncSender<TimerAction>,
    ) -> Result<(), Error> {
        let duration = self
            .internal()
            .manifest
            .as_ref()
            .timeout_ms
            .map(std::time::Duration::from_millis);
        self.cancel_handle.epoch_timer_tx = Some(tx.clone());
        self.store_mut().set_epoch_deadline(1);
        let engine: Engine = self.store().engine().clone();
        tx.send(TimerAction::Start {
            id: self.timer_id,
            duration,
            engine,
        })?;
        Ok(())
    }

    /// Send TimerAction::Stop
    pub(crate) fn stop_timer(&mut self) -> Result<(), Error> {
        if let Some(tx) = &self.cancel_handle.epoch_timer_tx {
            tx.send(TimerAction::Stop { id: self.timer_id })?;
        }
        Ok(())
    }
}

// Enumerates the supported PDK language runtimes
#[derive(Clone)]
pub(crate) enum Runtime {
    Haskell { init: Func, cleanup: Func },
    Wasi { init: Func, cleanup: Option<Func> },
}
