use crate::*;

enum Source {
    Manifest(Manifest),
    Data(Vec<u8>),
}

/// PluginBuilder is used to configure and create `Plugin` instances
pub struct PluginBuilder {
    source: Source,
    wasi: bool,
    functions: Vec<Function>,
}

impl PluginBuilder {
    /// Create a new `PluginBuilder` with the given WebAssembly module
    pub fn new_with_module(data: impl Into<Vec<u8>>) -> Self {
        PluginBuilder {
            source: Source::Data(data.into()),
            wasi: false,
            functions: vec![],
        }
    }

    /// Create a new `PluginBuilder` from a `Manifest`
    pub fn new(manifest: Manifest) -> Self {
        PluginBuilder {
            source: Source::Manifest(manifest),
            wasi: false,
            functions: vec![],
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

    /// Generate a new plugin with the configured settings
    pub fn build<'a>(self) -> Result<Plugin, Error> {
        match self.source {
            Source::Manifest(m) => Plugin::new_with_manifest(&m, self.functions, self.wasi),
            Source::Data(d) => Plugin::new(d, self.functions, self.wasi),
        }
    }
}
