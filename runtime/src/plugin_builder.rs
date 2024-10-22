use std::path::PathBuf;

use crate::{plugin::WasmInput, *};

#[derive(Clone)]
pub struct DebugOptions {
    pub profiling_strategy: wasmtime::ProfilingStrategy,
    pub coredump: Option<std::path::PathBuf>,
    pub memdump: Option<std::path::PathBuf>,
    pub debug_info: bool,
}

impl Default for DebugOptions {
    fn default() -> Self {
        let debug_info = std::env::var("EXTISM_DEBUG").is_ok();
        let coredump = if let Ok(x) = std::env::var("EXTISM_COREDUMP") {
            Some(std::path::PathBuf::from(x))
        } else {
            None
        };
        let memdump = if let Ok(x) = std::env::var("EXTISM_MEMDUMP") {
            Some(std::path::PathBuf::from(x))
        } else {
            None
        };
        DebugOptions {
            profiling_strategy: plugin::profiling_strategy(),
            coredump,
            memdump,
            debug_info,
        }
    }
}

/// PluginBuilder is used to configure and create `Plugin` instances
#[derive(Clone)]
pub struct PluginBuilder<'a> {
    pub(crate) source: WasmInput<'a>,
    pub(crate) wasi: bool,
    pub(crate) functions: Vec<Function>,
    pub(crate) debug_options: DebugOptions,
    pub(crate) cache_config: Option<Option<PathBuf>>,
    pub(crate) fuel: Option<u64>,
    pub(crate) config: Option<wasmtime::Config>,
    pub(crate) http_response_headers: bool,
}

impl<'a> PluginBuilder<'a> {
    /// Create a new `PluginBuilder` from a `Manifest` or raw Wasm bytes
    pub fn new(plugin: impl Into<WasmInput<'a>>) -> Self {
        PluginBuilder {
            source: plugin.into(),
            wasi: false,
            functions: vec![],
            debug_options: DebugOptions::default(),
            cache_config: None,
            fuel: None,
            config: None,
            http_response_headers: false,
        }
    }

    /// Enables WASI if the argument is set to `true`
    pub fn with_wasi(mut self, wasi: bool) -> Self {
        self.wasi = wasi;
        self
    }

    /// Add a single host function
    pub fn with_function<T: 'static, F>(
        mut self,
        name: impl Into<String>,
        args: impl IntoIterator<Item = ValType>,
        returns: impl IntoIterator<Item = ValType>,
        user_data: UserData<T>,
        f: F,
    ) -> Self
    where
        F: 'static
            + Fn(&mut CurrentPlugin, &[Val], &mut [Val], UserData<T>) -> Result<(), Error>
            + Sync
            + Send,
    {
        self.functions
            .push(Function::new(name, args, returns, user_data, f));
        self
    }

    /// Add a single host function in a specific namespace
    pub fn with_function_in_namespace<T: 'static, F>(
        mut self,
        namespace: impl Into<String>,
        name: impl Into<String>,
        args: impl IntoIterator<Item = ValType>,
        returns: impl IntoIterator<Item = ValType>,
        user_data: UserData<T>,
        f: F,
    ) -> Self
    where
        F: 'static
            + Fn(&mut CurrentPlugin, &[Val], &mut [Val], UserData<T>) -> Result<(), Error>
            + Sync
            + Send,
    {
        self.functions
            .push(Function::new(name, args, returns, user_data, f).with_namespace(namespace));
        self
    }

    /// Add multiple host functions
    pub fn with_functions(mut self, f: impl IntoIterator<Item = Function>) -> Self {
        self.functions.extend(f);
        self
    }

    /// Set profiling strategy
    pub fn with_profiling_strategy(mut self, p: wasmtime::ProfilingStrategy) -> Self {
        self.debug_options.profiling_strategy = p;
        self
    }

    /// Enable Wasmtime coredump on trap
    pub fn with_coredump(mut self, path: impl Into<std::path::PathBuf>) -> Self {
        self.debug_options.coredump = Some(path.into());
        self
    }

    /// Enable Extism memory dump when plugin calls return an error
    pub fn with_memdump(mut self, path: impl Into<std::path::PathBuf>) -> Self {
        self.debug_options.memdump = Some(path.into());
        self
    }

    /// Compile with debug info
    pub fn with_debug_info(mut self) -> Self {
        self.debug_options.debug_info = true;
        self
    }

    /// Configure debug options
    pub fn with_debug_options(mut self, options: DebugOptions) -> Self {
        self.debug_options = options;
        self
    }

    /// Set wasmtime compilation cache config path
    pub fn with_cache_config(mut self, dir: impl Into<PathBuf>) -> Self {
        self.cache_config = Some(Some(dir.into()));
        self
    }

    /// Turn wasmtime compilation caching off
    pub fn with_cache_disabled(mut self) -> Self {
        self.cache_config = Some(None);
        self
    }

    /// Limit the number of instructions that can be executed
    pub fn with_fuel_limit(mut self, fuel: u64) -> Self {
        self.fuel = Some(fuel);
        self
    }

    /// Configure an initial wasmtime config to be passed to the plugin
    ///
    /// **Warning**: some values might be overwritten by the Extism runtime. In particular:
    /// - async_support
    /// - epoch_interruption
    /// - debug_info
    /// - coredump_on_trap
    /// - profiler
    /// - wasm_tail_call
    /// - wasm_function_references
    /// - wasm_gc
    ///
    /// See the implementation details of [PluginBuilder::build] and [Plugin::build_new] to verify which values are overwritten.
    pub fn with_wasmtime_config(mut self, config: wasmtime::Config) -> Self {
        self.config = Some(config);
        self
    }

    /// Enables `http_response_headers`, which allows for plugins to access response headers when using `extism:host/env::http_request`
    pub fn with_http_response_headers(mut self, allow: bool) -> Self {
        self.http_response_headers = allow;
        self
    }

    /// Generate a new plugin with the configured settings
    pub fn build(self) -> Result<Plugin, Error> {
        Plugin::build_new(self)
    }
}
