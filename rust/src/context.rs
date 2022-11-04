use crate::*;

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

    /// Remove all registered plugins
    pub fn reset(&mut self) {
        unsafe { bindings::extism_context_reset(&mut *self.lock()) }
    }

    pub(crate) fn lock<'a>(&'a self) -> std::sync::MutexGuard<'a, extism_runtime::Context> {
        match self.0.lock() {
            Ok(x) => x,
            Err(x) => x.into_inner(),
        }
    }
}
