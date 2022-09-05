use crate::*;

pub struct Context {
    pub(crate) pointer: *mut bindings::ExtismContext,
}

impl Default for Context {
    fn default() -> Context {
        Context::new()
    }
}

impl Context {
    /// Create a new context
    pub fn new() -> Context {
        let pointer = unsafe { bindings::extism_context_new() };
        Context { pointer }
    }

    /// Remove all registered plugins
    pub fn reset(&mut self) {
        unsafe { bindings::extism_context_reset(self.pointer) }
    }
}

unsafe impl Send for Context {}
unsafe impl Sync for Context {}

impl Drop for Context {
    fn drop(&mut self) {
        if self.pointer.is_null() {
            return;
        }
        unsafe { bindings::extism_context_free(self.pointer) }
        self.pointer = std::ptr::null_mut();
    }
}
