use std::collections::BTreeMap;

use crate::*;

/// Plugin contains everything needed to execute a WASM function
pub struct Plugin {
    pub module: Module,
    pub linker: Linker<Internal>,
    pub instance: Instance,
    pub last_error: std::cell::RefCell<Option<std::ffi::CString>>,
    pub memory: PluginMemory,
    pub manifest: Manifest,
    pub vars: BTreeMap<String, Vec<u8>>,
    pub timer_id: uuid::Uuid,
    pub(crate) cancel_handle: sdk::ExtismCancelHandle,
    pub(crate) runtime: Option<Runtime>,
}

pub struct Internal {
    pub input_length: usize,
    pub input: *const u8,
    pub output_offset: usize,
    pub output_length: usize,
    pub plugin: *mut Plugin,
    pub wasi: Option<Wasi>,
    pub http_status: u16,
}

pub struct Wasi {
    pub ctx: wasmtime_wasi::WasiCtx,
    #[cfg(feature = "nn")]
    pub nn: wasmtime_wasi_nn::WasiNnCtx,
    #[cfg(not(feature = "nn"))]
    pub nn: (),
}

impl Internal {
    fn new(manifest: &Manifest, wasi: bool) -> Result<Self, Error> {
        let wasi = if wasi {
            let auth = wasmtime_wasi::ambient_authority();
            let mut ctx = wasmtime_wasi::WasiCtxBuilder::new();
            for (k, v) in manifest.as_ref().config.iter() {
                ctx = ctx.env(k, v)?;
            }

            if let Some(a) = &manifest.as_ref().allowed_paths {
                for (k, v) in a.iter() {
                    let d = wasmtime_wasi::Dir::open_ambient_dir(k, auth)?;
                    ctx = ctx.preopened_dir(d, v)?;
                }
            }

            #[cfg(feature = "nn")]
            let nn = wasmtime_wasi_nn::WasiNnCtx::new()?;

            #[cfg(not(feature = "nn"))]
            #[allow(clippy::let_unit_value)]
            let nn = ();

            Some(Wasi {
                ctx: ctx.build(),
                nn,
            })
        } else {
            None
        };

        Ok(Internal {
            input_length: 0,
            output_offset: 0,
            output_length: 0,
            input: std::ptr::null(),
            wasi,
            plugin: std::ptr::null_mut(),
            http_status: 0,
        })
    }

    pub fn plugin(&self) -> &Plugin {
        unsafe { &*self.plugin }
    }

    pub fn plugin_mut(&mut self) -> &mut Plugin {
        unsafe { &mut *self.plugin }
    }

    pub fn memory(&self) -> &PluginMemory {
        &self.plugin().memory
    }

    pub fn memory_mut(&mut self) -> &mut PluginMemory {
        &mut self.plugin_mut().memory
    }
}

const EXPORT_MODULE_NAME: &str = "env";

impl Plugin {
    /// Create a new plugin from the given WASM code
    pub fn new<'a>(
        wasm: impl AsRef<[u8]>,
        imports: impl IntoIterator<Item = &'a Function>,
        with_wasi: bool,
    ) -> Result<Plugin, Error> {
        let engine = Engine::new(
            Config::new()
                .epoch_interruption(true)
                .debug_info(std::env::var("EXTISM_DEBUG").is_ok()),
        )?;
        let mut imports = imports.into_iter();
        let (manifest, modules) = Manifest::new(&engine, wasm.as_ref())?;
        let mut store = Store::new(&engine, Internal::new(&manifest, with_wasi)?);

        store.epoch_deadline_callback(|_internal| Err(Error::msg("timeout")));

        let memory = Memory::new(
            &mut store,
            MemoryType::new(4, manifest.as_ref().memory.max_pages),
        )?;
        let mut memory = PluginMemory::new(store, memory);

        let mut linker = Linker::new(&engine);
        linker.allow_shadowing(true);

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

        macro_rules! define_funcs {
            ($m:expr, { $($name:ident($($args:expr),*) $(-> $($r:expr),*)?);* $(;)?}) => {
                match $m {
                $(
                    concat!("extism_", stringify!($name)) => {
                        let t = FuncType::new([$($args),*], [$($($r),*)?]);
                        let f = Func::new(&mut memory.store, t, pdk::$name);
                        linker.define(&mut memory.store, EXPORT_MODULE_NAME, concat!("extism_", stringify!($name)), Extern::Func(f))?;
                        continue
                    }
                )*
                    _ => ()
                }
            };
        }

        // Add builtins
        for (_name, module) in modules.iter() {
            for import in module.imports() {
                let module_name = import.module();
                let name = import.name();
                use wasmtime::ValType::*;

                if module_name == EXPORT_MODULE_NAME {
                    define_funcs!(name,  {
                        alloc(I64) -> I64;
                        free(I64);
                        load_u8(I64) -> I32;
                        load_u64(I64) -> I64;
                        store_u8(I64, I32);
                        store_u64(I64, I64);
                        input_length() -> I64;
                        input_load_u8(I64) -> I32;
                        input_load_u64(I64) -> I64;
                        output_set(I64, I64);
                        error_set(I64);
                        config_get(I64) -> I64;
                        var_get(I64) -> I64;
                        var_set(I64, I64);
                        http_request(I64, I64) -> I64;
                        http_status_code() -> I32;
                        length(I64) -> I64;
                        log_warn(I64);
                        log_info(I64);
                        log_debug(I64);
                        log_error(I64);
                    });

                    for f in &mut imports {
                        let name = f.name().to_string();
                        let ns = f.namespace().unwrap_or(EXPORT_MODULE_NAME);
                        let func = Func::new(&mut memory.store, f.ty().clone(), unsafe {
                            &*std::sync::Arc::as_ptr(&f.f)
                        });
                        linker.define(&mut memory.store, ns, &name, func)?;
                    }
                }
            }
        }

        // Add modules to linker
        for (name, module) in modules.iter() {
            if name != main_name {
                linker.module(&mut memory.store, name, module)?;
                linker.alias_module(name, "env")?;
            }
        }

        let instance = linker.instantiate(&mut memory.store, main)?;
        let timer_id = uuid::Uuid::new_v4();
        let mut plugin = Plugin {
            module: main.clone(),
            linker,
            memory,
            instance,
            last_error: std::cell::RefCell::new(None),
            manifest,
            vars: BTreeMap::new(),
            runtime: None,
            timer_id,
            cancel_handle: sdk::ExtismCancelHandle {
                id: timer_id,
                epoch_timer_tx: None,
            },
        };
        plugin.detect_runtime();
        Ok(plugin)
    }

    /// Get a function by name
    pub fn get_func(&mut self, function: impl AsRef<str>) -> Option<Func> {
        self.instance
            .get_func(&mut self.memory.store, function.as_ref())
    }

    /// Set `last_error` field
    pub fn set_error(&self, e: impl std::fmt::Debug) {
        debug!("Set error: {:?}", e);
        *self.last_error.borrow_mut() = Some(error_string(e));
    }

    pub fn error<E>(&self, e: impl std::fmt::Debug, x: E) -> E {
        self.set_error(e);
        x
    }

    /// Unset `last_error` field
    pub fn clear_error(&self) {
        *self.last_error.borrow_mut() = None;
    }

    /// Store input in memory and initialize `Internal` pointer
    pub fn set_input(&mut self, input: *const u8, mut len: usize) {
        if input.is_null() {
            len = 0;
        }
        let ptr = self as *mut _;
        let internal = self.memory.store.data_mut();
        internal.input = input;
        internal.input_length = len;
        internal.plugin = ptr;
    }

    pub fn dump_memory(&self) {
        self.memory.dump();
    }

    pub fn reinstantiate(&mut self) -> Result<(), Error> {
        let instance = self
            .linker
            .instantiate(&mut self.memory.store, &self.module)?;
        self.instance = instance;
        self.detect_runtime();
        Ok(())
    }

    pub fn has_wasi(&self) -> bool {
        self.memory.store.data().wasi.is_some()
    }

    fn detect_runtime(&mut self) {
        // Check for Haskell runtime initialization functions
        // Initialize Haskell runtime if `hs_init` and `hs_exit` are present,
        // by calling the `hs_init` export
        if let Some(init) = self.get_func("hs_init") {
            if let Some(cleanup) = self.get_func("hs_exit") {
                if init.typed::<(i32, i32), ()>(&self.memory.store).is_err() {
                    trace!(
                        "hs_init function found with type {:?}",
                        init.ty(&self.memory.store)
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
                if init.typed::<(), ()>(&self.memory.store).is_err() {
                    trace!(
                        "__wasm_call_ctors function found with type {:?}",
                        init.ty(&self.memory.store)
                    );
                    return;
                }
                trace!("WASI runtime detected");
                if let Some(cleanup) = self.get_func("__wasm_call_dtors") {
                    if cleanup.typed::<(), ()>(&self.memory.store).is_err() {
                        trace!(
                            "__wasm_call_dtors function found with type {:?}",
                            cleanup.ty(&self.memory.store)
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
        if let Some(runtime) = &self.runtime {
            trace!("Plugin::initialize_runtime");
            match runtime {
                Runtime::Haskell { init, cleanup: _ } => {
                    let mut results =
                        vec![Val::null(); init.ty(&self.memory.store).results().len()];
                    init.call(
                        &mut self.memory.store,
                        &[Val::I32(0), Val::I32(0)],
                        results.as_mut_slice(),
                    )?;
                    debug!("Initialized Haskell language runtime");
                }
                Runtime::Wasi { init, cleanup: _ } => {
                    debug!("Calling __wasm_call_ctors");
                    init.call(&mut self.memory.store, &[], &mut [])?;
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
                    cleanup.call(&mut self.memory.store, &[], &mut [])?;
                }
                Runtime::Wasi {
                    init: _,
                    cleanup: None,
                } => (),
                // Cleanup Haskell runtime if `hs_exit` and `hs_exit` are present,
                // by calling the `hs_exit` export
                Runtime::Haskell { init: _, cleanup } => {
                    let mut results =
                        vec![Val::null(); cleanup.ty(&self.memory.store).results().len()];
                    cleanup.call(&mut self.memory.store, &[], results.as_mut_slice())?;
                    debug!("Cleaned up Haskell language runtime");
                }
            }
        }

        Ok(())
    }

    pub(crate) fn start_timer(
        &mut self,
        tx: &std::sync::mpsc::SyncSender<TimerAction>,
    ) -> Result<(), Error> {
        let duration = self
            .manifest
            .as_ref()
            .timeout_ms
            .map(std::time::Duration::from_millis);
        self.cancel_handle.epoch_timer_tx = Some(tx.clone());
        self.memory.store.set_epoch_deadline(1);
        let engine: Engine = self.memory.store.engine().clone();
        tx.send(TimerAction::Start {
            id: self.timer_id,
            duration,
            engine,
        })?;
        Ok(())
    }

    pub(crate) fn stop_timer(&mut self) -> Result<(), Error> {
        if let Some(tx) = &self.cancel_handle.epoch_timer_tx {
            tx.send(TimerAction::Stop { id: self.timer_id })?;
        }
        Ok(())
    }

    pub fn cancel(&self) -> Result<(), Error> {
        if let Some(tx) = &self.cancel_handle.epoch_timer_tx {
            tx.send(TimerAction::Cancel { id: self.timer_id })?;
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
