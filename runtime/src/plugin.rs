use std::collections::BTreeMap;

use crate::*;

/// Plugin contains everything needed to execute a WASM function
pub struct Plugin {
    pub module: Module,
    pub linker: Linker<Internal>,
    pub instance: Instance,
    pub last_error: Option<std::ffi::CString>,
    pub memory: PluginMemory,
    pub manifest: Manifest,
}

pub struct Internal {
    pub input_offset: usize,
    pub input_length: usize,
    pub output_offset: usize,
    pub output_length: usize,
    pub vars: BTreeMap<String, Vec<u8>>,
    pub wasi: wasmtime_wasi::WasiCtx,
    pub plugin: *mut Plugin,
}

impl Internal {
    fn new(manifest: &Manifest) -> Result<Self, Error> {
        let mut wasi = wasmtime_wasi::WasiCtxBuilder::new();
        for (k, v) in manifest.as_ref().config.iter() {
            wasi = wasi.env(k, v)?;
        }
        Ok(Internal {
            input_offset: 0,
            input_length: 0,
            output_offset: 0,
            output_length: 0,
            wasi: wasi.build(),
            vars: BTreeMap::new(),
            plugin: std::ptr::null_mut(),
        })
    }
}

const EXPORT_MODULE_NAME: &str = "env";

impl Plugin {
    /// Create a new plugin from the given WASM code
    pub fn new(wasm: impl AsRef<[u8]>, with_wasi: bool) -> Result<Plugin, Error> {
        let engine = Engine::default();
        let (manifest, modules) = Manifest::new(&engine, wasm.as_ref())?;
        let mut store = Store::new(&engine, Internal::new(&manifest)?);
        let memory = Memory::new(&mut store, MemoryType::new(4, manifest.as_ref().memory.max))?;
        let mut memory = PluginMemory::new(store, memory);

        let mut linker = Linker::new(&engine);
        linker.allow_shadowing(true);

        if with_wasi {
            wasmtime_wasi::add_to_linker(&mut linker, |x: &mut Internal| &mut x.wasi)?;
        }

        // Get the `main` module, or the last one if `main` doesn't exist
        let (main_name, main) = modules.get("main").map(|x| ("main", x)).unwrap_or_else(|| {
            let entry = modules.iter().last().unwrap();
            (entry.0.as_str(), entry.1)
        });

        // Collect exports
        let mut exports = BTreeMap::new();
        for (_name, module) in modules.iter() {
            for export in module.exports() {
                exports.insert(export.name(), export);
            }
        }

        macro_rules! define_funcs {
            ($m:expr, { $($name:ident($($args:expr),*) $(-> $($r:expr),*)?);* $(;)?}) => {
                match $m {
                $(
                    concat!("extism_", stringify!($name)) => {
                        let t = FuncType::new([$($args),*], [$($($r),*)?]);
                        let f = Func::new(&mut memory.store, t, export::$name);
                        linker.define(EXPORT_MODULE_NAME, concat!("extism_", stringify!($name)), Extern::Func(f))?;
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
                use ValType::*;

                if module_name == EXPORT_MODULE_NAME {
                    define_funcs!(name,  {
                        alloc(I64) -> I64;
                        free(I64);
                        load_u8(I64) -> I32;
                        load_u32(I64) -> I32;
                        load_u64(I64) -> I64;
                        store_u8(I64, I32);
                        store_u32(I64, I32);
                        store_u64(I64, I64);
                        input_offset() -> I64;
                        output_set(I64, I64);
                        error_set(I64);
                        config_get(I64) -> I64;
                        var_get(I64) -> I64;
                        var_set(I64, I64);
                        http_request(I64, I64) -> I64;
                        length(I64) -> I64;
                    });
                }

                // Define memory or check to ensure the symbol is exported by another module
                // since it doesn't match one of our known exports
                if !module_name.starts_with("wasi") && !exports.contains_key(name) {
                    return Err(anyhow::format_err!("Invalid export: {module_name}::{name}"));
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

        Ok(Plugin {
            module: main.clone(),
            linker,
            memory,
            instance,
            last_error: None,
            manifest,
        })
    }

    /// Get a function by name
    pub fn get_func(&mut self, function: impl AsRef<str>) -> Option<Func> {
        self.instance
            .get_func(&mut self.memory.store, function.as_ref())
    }

    /// Set `last_error` field
    pub fn set_error(&mut self, e: impl std::fmt::Debug) {
        let x = format!("{:?}", e).into_bytes();
        let x = if x[0] == b'"' && x[x.len() - 1] == b'"' {
            x[1..x.len() - 1].to_vec()
        } else {
            x
        };
        let e = unsafe { std::ffi::CString::from_vec_unchecked(x) };
        self.last_error = Some(e);
    }

    pub fn error<E>(&mut self, e: impl std::fmt::Debug, x: E) -> E {
        self.set_error(e);
        x
    }

    /// Unset `last_error` field
    pub fn clear_error(&mut self) {
        self.last_error = None;
    }

    /// Store input in memory and initialize `Internal` pointer
    pub fn set_input(&mut self, handle: MemoryBlock) {
        let ptr = self as *mut _;
        let internal = self.memory.store.data_mut();
        internal.input_offset = handle.offset;
        internal.input_length = handle.length;
        internal.plugin = ptr;
    }

    #[cfg(feature = "debug")]
    pub fn dump_memory(&self) {
        self.memory.dump();
    }
}

/// A registry for plugins
pub static mut PLUGINS: std::sync::Mutex<Vec<Plugin>> = std::sync::Mutex::new(Vec::new());
