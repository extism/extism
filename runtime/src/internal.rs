use std::collections::BTreeMap;

use crate::*;

impl wasmtime::ResourceLimiter for Internal {
    fn memory_growing(
        &mut self,
        current: usize,
        desired: usize,
        maximum: Option<usize>,
    ) -> Result<bool> {
        let new = desired - current;
        let mut ok = true;

        if let Some(available) = &mut self.available_pages {
            if new > *available as usize * 65536 {
                ok = false;
            } else {
                *available -= (new / 65536) as u32;
            }
        }

        // if let Some(max) = self.manifest.as_ref().memory.max_pages {
        //     ok = ok && max as usize * 65536 >= desired
        // }

        if let Some(max) = maximum {
            ok = ok && desired <= max;
        }

        Ok(ok)
    }

    fn table_growing(&mut self, _current: u32, desired: u32, maximum: Option<u32>) -> Result<bool> {
        if let Some(max) = maximum {
            return Ok(max >= desired);
        }

        Ok(true)
    }
}

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

    /// Store plugin-specific error messages
    // pub last_error: std::cell::RefCell<Option<std::ffi::CString>>,

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

    pub fn set_error(&mut self, e: impl std::fmt::Debug) {
        debug!("Set error: {:?}", e);
        let s = format!("{e:?}");
        let output = &mut [Val::I64(0)];
        let mut store = unsafe { &mut *self.store };
        self.linker_mut()
            .get(&mut store, "env", "extism_alloc")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[Val::I64(s.len() as i64)], &mut [])
            .unwrap();

        self.linker_mut()
            .get(&mut store, "env", "memory")
            .unwrap()
            .into_memory()
            .unwrap()
            .write(&mut store, output[0].unwrap_i64() as usize, s.as_bytes())
            .unwrap();

        self.linker_mut()
            .get(&mut store, "env", "extism_error_set")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, output, &mut [])
            .unwrap();
    }

    /// Unset `last_error` field
    pub fn clear_error(&mut self) {
        // let mut store = unsafe { &mut *self.store };
        // self.linker_mut()
        //     .get(&mut store, "env", "extism_error_set")
        //     .unwrap()
        //     .into_func()
        //     .unwrap()
        //     .call(&mut store, &[Val::I64(0)], &mut [])
        //     .unwrap();
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
