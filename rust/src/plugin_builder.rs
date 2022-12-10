use crate::*;

enum Source {
    Manifest(Manifest),
    Data(Vec<u8>),
}

/// PluginBuilder is used to configure and create `Plugin` instances
pub struct PluginBuilder {
    source: Source,
    wasi: bool,
    functions: Vec<&'static Function>,
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
    pub fn with_function(mut self, f: &'static Function) -> Self {
        self.functions.push(f);
        self
    }

    /// Add multiple host functions
    pub fn with_functions(mut self, f: impl IntoIterator<Item = &'static Function>) -> Self {
        self.functions.extend(f);
        self
    }

    pub fn build(self, context: &Context) -> Result<Plugin, Error> {
        match self.source {
            Source::Manifest(m) => {
                Plugin::new_with_manifest_and_functions(context, &m, self.functions, self.wasi)
            }
            Source::Data(d) => Plugin::new_with_functions(context, &d, self.functions, self.wasi),
        }
    }
}
