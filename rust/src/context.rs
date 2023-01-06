use extism_runtime::PluginIndex;

use crate::{
    plugin_builder::{PluginScheme, Source},
    *,
};

pub struct Context(pub(crate) std::sync::Arc<std::sync::Mutex<extism_runtime::Context>>);

impl Default for Context {
    fn default() -> Context {
        Context::new()
    }
}

impl Context {
    /// Create a new context
    pub fn new() -> Context {
        Context(std::sync::Arc::new(std::sync::Mutex::new(
            extism_runtime::Context::new(),
        )))
    }

    pub fn get(&self, id: PluginIndex) -> Option<&mut extism_runtime::Plugin> {
        // TODO: an example impl
        // self.lock().plugin(id)
        todo!()
    }

    pub fn insert(&self, scheme: PluginScheme) -> Result<Plugin, Error> {
        match scheme.source {
            Source::Manifest(m) => {
                Plugin::new_with_manifest_and_functions(&self, &m, scheme.functions, scheme.wasi)
            }
            Source::Data(d) => Plugin::new_with_functions(&self, &d, scheme.functions, scheme.wasi),
        }
    }

    pub fn remove(&mut self, plugin: Plugin<'_>) {
        unsafe { bindings::extism_plugin_free(&mut *self.lock(), plugin.id) }
    }

    /// Remove all registered plugins
    pub fn reset(&mut self) {
        unsafe { bindings::extism_context_reset(&mut *self.lock()) }
    }

    pub(crate) fn lock(&self) -> std::sync::MutexGuard<extism_runtime::Context> {
        match self.0.lock() {
            Ok(x) => x,
            Err(x) => x.into_inner(),
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        self.reset();
    }
}
