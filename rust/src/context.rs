use crate::*;

pub struct Context(pub(crate) std::cell::RefCell<extism_runtime::Context>);

impl Default for Context {
    fn default() -> Context {
        Context::new()
    }
}

impl Context {
    /// Create a new context
    pub fn new() -> Context {
        Context(std::cell::RefCell::new(extism_runtime::Context::new()))
    }

    /// Remove all registered plugins
    pub fn reset(&mut self) {
        unsafe { bindings::extism_context_reset(self.0.as_ptr()) }
    }
}
