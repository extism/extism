use crate::*;

/// WASI context
pub struct Wasi {
    /// wasi
    pub ctx: wasmtime_wasi::preview2::WasiCtx,
    pub preview2_table: wasmtime::component::ResourceTable,
    pub preview1_adapter: wasmtime_wasi::preview2::preview1::WasiPreview1Adapter,
}

impl wasmtime_wasi::preview2::WasiView for CurrentPlugin {
    fn table(&mut self) -> &mut wasmtime::component::ResourceTable {
        &mut self.wasi.as_mut().unwrap().preview2_table
    }

    fn ctx(&mut self) -> &mut wasmtime_wasi::preview2::WasiCtx {
        &mut self.wasi.as_mut().unwrap().ctx
    }
}

impl wasmtime_wasi::preview2::preview1::WasiPreview1View for CurrentPlugin {
    fn adapter(&self) -> &wasmtime_wasi::preview2::preview1::WasiPreview1Adapter {
        &self.wasi.as_ref().unwrap().preview1_adapter
    }

    fn adapter_mut(&mut self) -> &mut wasmtime_wasi::preview2::preview1::WasiPreview1Adapter {
        &mut self.wasi.as_mut().unwrap().preview1_adapter
    }
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
