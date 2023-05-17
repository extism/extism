use crate::*;

#[derive(Clone)]
pub struct Context(pub(crate) std::sync::Arc<std::sync::Mutex<extism_runtime::Context>>);

impl Default for Context {
    fn default() -> Context {
        Context::new()
    }
}

unsafe impl Sync for Context {}
unsafe impl Send for Context {}

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

    pub(crate) fn lock(&self) -> std::sync::MutexGuard<extism_runtime::Context> {
        match self.0.lock() {
            Ok(x) => x,
            Err(x) => x.into_inner(),
        }
    }
}
