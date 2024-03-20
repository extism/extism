use crate::*;

/// WASI context
pub struct Wasi {
    /// wasi
    pub ctx: wasmtime_wasi::WasiP1Ctx,
}

/// InternalExt provides a unified way of acessing `memory`, `store` and `internal` values
pub(crate) trait Internal {
    fn store(&self) -> &Store<CurrentPlugin>;

    fn store_mut(&mut self) -> &mut Store<CurrentPlugin>;

    fn linker(&self) -> &Linker<CurrentPlugin>;

    fn linker_mut(&mut self) -> &mut Linker<CurrentPlugin>;

    fn linker_and_store(&mut self) -> (&mut Linker<CurrentPlugin>, &mut Store<CurrentPlugin>);

    fn current_plugin(&self) -> &CurrentPlugin {
        self.store().data()
    }

    fn current_plugin_mut(&mut self) -> &mut CurrentPlugin {
        self.store_mut().data_mut()
    }
}
