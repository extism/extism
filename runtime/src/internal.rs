use std::collections::BTreeMap;

use crate::*;

/// WASI context
pub struct Wasi {
    /// wasi
    pub ctx: wasmtime_wasi::WasiCtx,

    /// wasi-nn
    #[cfg(feature = "nn")]
    pub nn: wasmtime_wasi_nn::WasiNnCtx,
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

    /// Plugin variables
    pub vars: BTreeMap<String, Vec<u8>>,

    pub manifest: Manifest,

    pub available_pages: Option<u32>,

    pub(crate) memory_limiter: Option<MemoryLimiter>,
}

/// InternalExt provides a unified way of acessing `memory`, `store` and `internal` values
pub trait InternalExt {
    fn store(&self) -> &Store<Internal>;

    fn store_mut(&mut self) -> &mut Store<Internal>;

    fn linker(&self) -> &Linker<Internal>;

    fn linker_mut(&mut self) -> &mut Linker<Internal>;

    fn linker_and_store(&mut self) -> (&mut Linker<Internal>, &mut Store<Internal>);

    fn internal(&self) -> &Internal {
        self.store().data()
    }

    fn internal_mut(&mut self) -> &mut Internal {
        self.store_mut().data_mut()
    }

    fn memory_ptr(&mut self) -> *mut u8 {
        let (linker, mut store) = self.linker_and_store();
        if let Some(mem) = linker.get(&mut store, "env", "memory") {
            if let Some(mem) = mem.into_memory() {
                return mem.data_ptr(&mut store);
            }
        }

        std::ptr::null_mut()
    }

    fn memory(&mut self) -> &mut [u8] {
        let (linker, mut store) = self.linker_and_store();
        let mem = linker
            .get(&mut store, "env", "memory")
            .unwrap()
            .into_memory()
            .unwrap();
        let ptr = mem.data_ptr(&store);
        if ptr.is_null() {
            return &mut [];
        }
        let size = mem.data_size(&store);
        unsafe { std::slice::from_raw_parts_mut(ptr, size) }
    }

    fn memory_read(&mut self, offs: u64, len: Size) -> &[u8] {
        let offs = offs as usize;
        let len = len as usize;
        let mem = self.memory();
        &mem[offs..offs + len]
    }

    fn memory_read_str(&mut self, offs: u64) -> Result<&str, std::str::Utf8Error> {
        let len = self.memory_length(offs);
        std::str::from_utf8(self.memory_read(offs, len))
    }

    fn memory_write(&mut self, offs: u64, bytes: impl AsRef<[u8]>) {
        let b = bytes.as_ref();
        let offs = offs as usize;
        let len = b.len();
        self.memory()[offs..offs + len].copy_from_slice(bytes.as_ref());
    }

    fn memory_alloc(&mut self, n: Size) -> Result<u64, Error> {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        linker
            .get(&mut store, "env", "extism_alloc")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[Val::I64(n as i64)], output)?;
        let offs = output[0].unwrap_i64() as u64;
        if offs == 0 {
            anyhow::bail!("out of memory")
        }
        Ok(offs)
    }

    fn memory_alloc_bytes(&mut self, bytes: impl AsRef<[u8]>) -> Result<u64, Error> {
        let b = bytes.as_ref();
        let offs = self.memory_alloc(b.len() as Size)?;
        self.memory_write(offs, b);
        Ok(offs)
    }

    fn memory_free(&mut self, offs: u64) {
        let (linker, mut store) = self.linker_and_store();
        linker
            .get(&mut store, "env", "extism_free")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[Val::I64(offs as i64)], &mut [])
            .unwrap();
    }

    fn memory_length(&mut self, offs: u64) -> u64 {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        linker
            .get(&mut store, "env", "extism_length")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[Val::I64(offs as i64)], output)
            .unwrap();
        output[0].unwrap_i64() as u64
    }

    // A convenience method to set the plugin error and return a value
    fn error<E>(&mut self, e: impl std::fmt::Debug, x: E) -> E {
        let s = format!("{e:?}");
        debug!("Set error: {:?}", s);
        if let Ok(offs) = self.memory_alloc_bytes(&s) {
            let (linker, mut store) = self.linker_and_store();
            if let Some(f) = linker.get(&mut store, "env", "extism_error_set") {
                f.into_func()
                    .unwrap()
                    .call(&mut store, &[Val::I64(offs as i64)], &mut [])
                    .unwrap();
            }
        }
        x
    }

    fn clear_error(&mut self) {
        let (linker, mut store) = self.linker_and_store();
        if let Some(f) = linker.get(&mut store, "env", "extism_error_set") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(0)], &mut [])
                .unwrap();
        }
    }

    fn has_error(&mut self) -> bool {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        linker
            .get(&mut store, "env", "extism_error_get")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[], output)
            .unwrap();
        output[0].unwrap_i64() != 0
    }

    fn get_error(&mut self) -> Option<&str> {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        linker
            .get(&mut store, "env", "extism_error_get")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[], output)
            .unwrap();
        let offs = output[0].unwrap_i64() as u64;
        if offs == 0 {
            return None;
        }

        let length = self.memory_length(offs);
        let data = self.memory_read(offs, length);
        let s = std::str::from_utf8(data);
        match s {
            Ok(s) => Some(s),
            Err(_) => None,
        }
    }
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

            Some(Wasi {
                ctx: ctx.build(),
                #[cfg(feature = "nn")]
                nn,
            })
        } else {
            None
        };

        let memory_limiter = if let Some(pgs) = available_pages {
            let n = pgs as usize * 65536;
            Some(MemoryLimiter {
                max_bytes: n,
                bytes_left: n,
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
            memory_limiter,
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

    fn linker(&self) -> &Linker<Internal> {
        unsafe { &*self.linker }
    }

    fn linker_mut(&mut self) -> &mut Linker<Internal> {
        unsafe { &mut *self.linker }
    }

    fn linker_and_store(&mut self) -> (&mut Linker<Internal>, &mut Store<Internal>) {
        unsafe { (&mut *self.linker, &mut *self.store) }
    }
}

pub(crate) struct MemoryLimiter {
    bytes_left: usize,
    max_bytes: usize,
}

impl MemoryLimiter {
    pub(crate) fn reset(&mut self) {
        self.bytes_left = self.max_bytes;
    }
}

impl wasmtime::ResourceLimiter for MemoryLimiter {
    fn memory_growing(
        &mut self,
        current: usize,
        desired: usize,
        maximum: Option<usize>,
    ) -> Result<bool> {
        if let Some(max) = maximum {
            if desired > max {
                return Ok(false);
            }
        }

        let d = desired - current;
        if d > self.bytes_left {
            return Ok(false);
        }

        self.bytes_left -= d;
        Ok(true)
    }

    fn table_growing(&mut self, _current: u32, desired: u32, maximum: Option<u32>) -> Result<bool> {
        if let Some(max) = maximum {
            return Ok(desired <= max);
        }

        Ok(true)
    }
}
