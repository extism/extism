use std::collections::BTreeMap;

use crate::*;

/// Internal stores data that is available to the caller in PDK functions
pub struct Internal {
    /// Call input length
    pub input_length: usize,

    /// Pointer to call input
    pub input: *const u8,

    /// Memory offset that points to the output
    pub output_offset: usize,

    /// Length of output in memory
    pub output_length: usize,

    /// WASI context
    pub wasi: Option<Wasi>,

    /// Keep track of the status from the last HTTP request
    pub http_status: u16,

    /// Store plugin-specific error messages
    pub last_error: std::cell::RefCell<Option<std::ffi::CString>>,

    /// Plugin variables
    pub vars: BTreeMap<String, Vec<u8>>,

    /// A pointer to the plugin memory, this should mostly be used from the PDK
    pub memory: *mut PluginMemory,

    pub(crate) available_pages: Option<u32>,
}

/// InternalExt provides a unified way of acessing `memory`, `store` and `internal` values
pub trait InternalExt {
    fn memory(&self) -> &PluginMemory;
    fn memory_mut(&mut self) -> &mut PluginMemory;

    fn store(&self) -> &Store<Internal> {
        self.memory().store()
    }

    fn store_mut(&mut self) -> &mut Store<Internal> {
        self.memory_mut().store_mut()
    }

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
        manifest: &Manifest,
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
            input_length: 0,
            output_offset: 0,
            output_length: 0,
            input: std::ptr::null(),
            wasi,
            memory: std::ptr::null_mut(),
            http_status: 0,
            last_error: std::cell::RefCell::new(None),
            vars: BTreeMap::new(),
            available_pages,
        })
    }

    pub fn set_error(&self, e: impl std::fmt::Debug) {
        debug!("Set error: {:?}", e);
        *self.last_error.borrow_mut() = Some(error_string(e));
    }

    /// Unset `last_error` field
    pub fn clear_error(&self) {
        *self.last_error.borrow_mut() = None;
    }
}

impl InternalExt for Internal {
    fn memory(&self) -> &PluginMemory {
        unsafe { &*self.memory }
    }

    fn memory_mut(&mut self) -> &mut PluginMemory {
        unsafe { &mut *self.memory }
    }
}
