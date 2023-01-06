use crate::*;

pub enum Source {
    Manifest(Manifest),
    Data(Vec<u8>),
}

pub struct PluginScheme {
    pub(crate) source: Source,
    pub(crate) wasi: bool,
    pub(crate) functions: Vec<Function>,
}

/// PluginSchemeBuilder is used to configure and create `Plugin` instances
pub struct PluginSchemeBuilder {
    source: Source,
    wasi: bool,
    functions: Vec<Function>,
}

impl PluginSchemeBuilder {
    /// Create a new `PluginSchemeBuilder` with the given WebAssembly module
    pub fn new_with_module(data: impl Into<Vec<u8>>) -> Self {
        PluginSchemeBuilder {
            source: Source::Data(data.into()),
            wasi: false,
            functions: vec![],
        }
    }

    /// Create a new `PluginSchemeBuilder` from a `Manifest`
    pub fn new(manifest: Manifest) -> Self {
        PluginSchemeBuilder {
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
    pub fn with_function(mut self, f: Function) -> Self {
        self.functions.push(f);
        self
    }

    /// Add multiple host functions
    pub fn with_functions(mut self, f: impl IntoIterator<Item = Function>) -> Self {
        self.functions.extend(f);
        self
    }

    pub fn build(self, ) -> PluginScheme {
        PluginScheme {
            source: self.source,
            wasi: self.wasi,
            functions: self.functions
        }
    }
}
