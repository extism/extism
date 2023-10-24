use crate::*;

enum Source {
    Manifest(Manifest),
    Data(Vec<u8>),
}

#[derive(Default, Clone)]
pub struct DebugOptions {
    pub(crate) profiling_strategy: Option<wasmtime::ProfilingStrategy>,
    pub(crate) coredump: Option<std::path::PathBuf>,
    pub(crate) memdump: Option<std::path::PathBuf>,
    pub(crate) debug_info: bool,
}

/// PluginBuilder is used to configure and create `Plugin` instances
pub struct PluginBuilder {
    source: Source,
    wasi: bool,
    functions: Vec<Function>,
    debug_options: DebugOptions,
}

impl PluginBuilder {
    /// Create a new `PluginBuilder` with the given WebAssembly module
    pub fn new_with_module(data: impl Into<Vec<u8>>) -> Self {
        PluginBuilder {
            source: Source::Data(data.into()),
            wasi: false,
            functions: vec![],
            debug_options: DebugOptions::default(),
        }
    }

    /// Create a new `PluginBuilder` from a `Manifest`
    pub fn new(manifest: Manifest) -> Self {
        PluginBuilder {
            source: Source::Manifest(manifest),
            wasi: false,
            functions: vec![],
            debug_options: DebugOptions::default(),
        }
    }

    /// Enables WASI if the argument is set to `true`
    pub fn with_wasi(mut self, wasi: bool) -> Self {
        self.wasi = wasi;
        self
    }

    /// Add a single host function
    pub fn with_function<F>(
        mut self,
        name: impl Into<String>,
        args: impl IntoIterator<Item = ValType>,
        returns: impl IntoIterator<Item = ValType>,
        user_data: Option<UserData>,
        f: F,
    ) -> Self
    where
        F: 'static
            + Fn(&mut CurrentPlugin, &[Val], &mut [Val], UserData) -> Result<(), Error>
            + Sync
            + Send,
    {
        self.functions.push(Function::new(
            name,
            args,
            returns,
            user_data.map(UserData::new),
            f,
        ));
        self
    }

    /// Add multiple host functions
    pub fn with_functions(mut self, f: impl IntoIterator<Item = Function>) -> Self {
        self.functions.extend(f);
        self
    }

    pub fn with_profiling_strategy(mut self, p: wasmtime::ProfilingStrategy) -> Self {
        self.debug_options.profiling_strategy = Some(p);
        self
    }

    pub fn with_coredump(mut self, path: impl AsRef<std::path::Path>) -> Self {
        self.debug_options.coredump = Some(path.as_ref().to_path_buf());
        self
    }

    pub fn with_memdump(mut self, path: impl AsRef<std::path::Path>) -> Self {
        self.debug_options.memdump = Some(path.as_ref().to_path_buf());
        self
    }

    pub fn with_debug_info(mut self) -> Self {
        self.debug_options.debug_info = true;
        self
    }

    /// Generate a new plugin with the configured settings
    pub fn build(self) -> Result<Plugin, Error> {
        match self.source {
            Source::Manifest(m) => {
                let data = serde_json::to_vec(&m)?;
                Plugin::build_new(&data, self.functions, self.wasi, self.debug_options)
            }
            Source::Data(d) => Plugin::build_new(d, self.functions, self.wasi, self.debug_options),
        }
    }
}
