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

    pub(crate) fn lock(&self) -> std::sync::MutexGuard<extism_runtime::Context> {
        match self.0.lock() {
            Ok(x) => x,
            Err(x) => x.into_inner(),
        }
    }

    pub fn current_plugin_memory(&mut self, mem: MemoryBlock) -> Option<*mut u8> {
        match self.lock().current_plugin() {
            Some(p) => unsafe { Some(p.memory.data_mut().as_mut_ptr().add(mem.offset)) },
            None => None,
        }
    }

    pub fn current_plugin_alloc(&mut self, n: usize) -> Result<MemoryBlock, Error> {
        match self.lock().current_plugin() {
            Some(p) => Ok(p.memory.alloc(n)?),
            None => Err(Error::Message("No active plugin".into())),
        }
    }

    pub fn current_plugin_free(&mut self, block: MemoryBlock) {
        match self.lock().current_plugin() {
            Some(p) => p.memory.free(block.offset),
            None => (),
        }
    }

    pub fn current_plugin_memory_block(&self, offs: usize) -> Option<MemoryBlock> {
        self.lock().current_plugin().and_then(|plugin| {
            plugin
                .memory
                .block_length(offs)
                .and_then(|len| Some(MemoryBlock::new(offs, len)))
        })
    }
}
