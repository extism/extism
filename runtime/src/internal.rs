use std::collections::BTreeMap;

use crate::*;

/// Internal stores data that is available to the caller in PDK functions
pub struct Internal {
    /// Store
    pub store: *mut Store<Internal>,

    /// Linker
    pub linker: *mut wasmtime::Linker<Internal>,

    /// WASI context
    pub wasi: Option<Wasi>,

    /// Keep track of the status from the last HTTP request
    pub http_status: u16,

    /// Plugin variables
    pub vars: BTreeMap<String, Vec<u8>>,

    pub manifest: Manifest,

    pub available_pages: Option<u32>,
}

/// InternalExt provides a unified way of acessing `memory`, `store` and `internal` values
pub trait InternalExt {
    fn store(&self) -> &Store<Internal>;

    fn store_mut(&mut self) -> &mut Store<Internal>;

    fn internal(&self) -> &Internal {
        self.store().data()
    }

    fn internal_mut(&mut self) -> &mut Internal {
        self.store_mut().data_mut()
    }
}

/// WASI context
pub struct Wasi {
    /// wasi
    pub ctx: wasmtime_wasi::WasiCtx,

    /// wasi-nn
    #[cfg(feature = "nn")]
    pub nn: wasmtime_wasi_nn::WasiNnCtx,
    #[cfg(not(feature = "nn"))]
    pub nn: (),
}

impl Internal {
    pub(crate) fn new(
        manifest: Manifest,
        wasi: bool,
        available_pages: Option<u32>,
    ) -> Result<Self, Error> {
        let wasi = if wasi {
            let auth = wasmtime_wasi::ambient_authority();
            let mut ctx = wasmtime_wasi::WasiCtxBuilder::new();
            for (k, v) in manifest.as_ref().config.iter() {
                ctx = ctx.env(k, v)?;
            }

            if let Some(a) = &manifest.as_ref().allowed_paths {
                for (k, v) in a.iter() {
                    let d = wasmtime_wasi::Dir::open_ambient_dir(k, auth)?;
                    ctx = ctx.preopened_dir(d, v)?;
                }
            }

            #[cfg(feature = "nn")]
            let nn = wasmtime_wasi_nn::WasiNnCtx::new()?;

            #[cfg(not(feature = "nn"))]
            #[allow(clippy::let_unit_value)]
            let nn = ();

            Some(Wasi {
                ctx: ctx.build(),
                nn,
            })
        } else {
            None
        };

        Ok(Internal {
            wasi,
            manifest,
            http_status: 0,
            vars: BTreeMap::new(),
            linker: std::ptr::null_mut(),
            store: std::ptr::null_mut(),
            available_pages,
        })
    }

    pub fn linker(&self) -> &wasmtime::Linker<Internal> {
        unsafe { &*self.linker }
    }

    pub fn linker_mut(&mut self) -> &mut wasmtime::Linker<Internal> {
        unsafe { &mut *self.linker }
    }
}

impl InternalExt for Internal {
    fn store(&self) -> &Store<Internal> {
        unsafe { &*self.store }
    }

    fn store_mut(&mut self) -> &mut Store<Internal> {
        unsafe { &mut *self.store }
    }
}
