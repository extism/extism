use crate::*;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct MemoryHandle(u64, u64);

impl MemoryHandle {
    /// Create a new memory handle, this is unsafe because the values are provided by the user
    /// and may not be a valid handle
    pub unsafe fn new(offs: u64, len: u64) -> MemoryHandle {
        MemoryHandle(offs, len)
    }

    /// Get the length of the memory block
    pub fn len(&self) -> usize {
        self.1 as usize
    }

    /// Get the offset to this block in Extism memory
    pub fn offset(&self) -> u64 {
        self.0
    }
}

impl From<MemoryHandle> for Val {
    fn from(m: MemoryHandle) -> Self {
        Val::I64(m.0 as i64)
    }
}

/// CurrentPlugin stores data that is available to the caller in PDK functions, this should
/// only be accessed from inside a host function
pub struct CurrentPlugin {
    /// Plugin variables
    pub(crate) vars: std::collections::BTreeMap<String, Vec<u8>>,

    /// Extism manifest
    pub(crate) manifest: extism_manifest::Manifest,
    pub(crate) store: *mut Store<CurrentPlugin>,
    pub(crate) linker: *mut wasmtime::Linker<CurrentPlugin>,
    pub(crate) wasi: Option<Wasi>,
    pub(crate) http_status: u16,
    pub(crate) available_pages: Option<u32>,
    pub(crate) memory_limiter: Option<MemoryLimiter>,
}

unsafe impl Sync for CurrentPlugin {}
unsafe impl Send for CurrentPlugin {}

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

impl CurrentPlugin {
    /// Access a plugin's variables
    pub fn vars(&self) -> &std::collections::BTreeMap<String, Vec<u8>> {
        &self.vars
    }

    /// Mutable access to a plugin's variables
    pub fn vars_mut(&mut self) -> &mut std::collections::BTreeMap<String, Vec<u8>> {
        &mut self.vars
    }

    /// Plugin manifest
    pub fn manifest(&self) -> &Manifest {
        &self.manifest
    }

    pub(crate) fn new(
        manifest: extism_manifest::Manifest,
        wasi: bool,
        available_pages: Option<u32>,
    ) -> Result<Self, Error> {
        let wasi = if wasi {
            let auth = wasmtime_wasi::ambient_authority();
            let mut ctx = wasmtime_wasi::WasiCtxBuilder::new();
            for (k, v) in manifest.config.iter() {
                ctx = ctx.env(k, v)?;
            }

            if let Some(a) = &manifest.allowed_paths {
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
            Some(crate::current_plugin::MemoryLimiter {
                max_bytes: n,
                bytes_left: n,
            })
        } else {
            None
        };

        Ok(CurrentPlugin {
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

    /// Get a pointer to the plugin memory
    pub fn memory_ptr(&mut self) -> *mut u8 {
        let (linker, mut store) = self.linker_and_store();
        if let Some(mem) = linker.get(&mut store, "env", "memory") {
            if let Some(mem) = mem.into_memory() {
                return mem.data_ptr(&mut store);
            }
        }

        std::ptr::null_mut()
    }

    /// Get a slice that contains the entire plugin memory
    pub fn memory(&mut self) -> &mut [u8] {
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

    /// Read a section of Extism plugin memory
    pub fn memory_read(&mut self, handle: MemoryHandle) -> &[u8] {
        trace!("memory_read: {}, {}", handle.0, handle.1);
        let offs = handle.0 as usize;
        let len = handle.1 as usize;
        let mem = self.memory();
        &mem[offs..offs + len]
    }

    /// Read a section of Extism plugin memory and convert to to an `str`
    pub fn memory_read_str(&mut self, handle: MemoryHandle) -> Result<&str, std::str::Utf8Error> {
        std::str::from_utf8(self.memory_read(handle))
    }

    /// Write data to an offset in Extism plugin memory
    pub fn memory_write(&mut self, handle: MemoryHandle, bytes: impl AsRef<[u8]>) {
        trace!("memory_write: {}", handle.0);
        let b = bytes.as_ref();
        let offs = handle.0 as usize;
        let len = b.len();
        self.memory()[offs..offs + len.min(handle.len())].copy_from_slice(bytes.as_ref());
    }

    /// Allocate a new block of Extism plugin memory
    pub fn memory_alloc(&mut self, n: Size) -> Result<MemoryHandle, Error> {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        if let Some(f) = linker.get(&mut store, "env", "extism_alloc") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(n as i64)], output)?;
        } else {
            anyhow::bail!("Unable to allocate memory");
        }
        let offs = output[0].unwrap_i64() as u64;
        if offs == 0 {
            anyhow::bail!("out of memory")
        }
        trace!("memory_alloc: {}, {}", offs, n);
        Ok(MemoryHandle(offs, n))
    }

    /// Allocate a new block in Extism plugin memory and fill it will the provided bytes
    pub fn memory_alloc_bytes(&mut self, bytes: impl AsRef<[u8]>) -> Result<MemoryHandle, Error> {
        let b = bytes.as_ref();
        let offs = self.memory_alloc(b.len() as Size)?;
        self.memory_write(offs, b);
        Ok(offs)
    }

    /// Free a block of Extism plugin memory
    pub fn memory_free(&mut self, handle: MemoryHandle) {
        let (linker, mut store) = self.linker_and_store();
        linker
            .get(&mut store, "env", "extism_free")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[Val::I64(handle.0 as i64)], &mut [])
            .unwrap();
    }

    /// Get a `MemoryHandle` from an offset
    pub fn memory_handle(&mut self, offs: u64) -> Option<MemoryHandle> {
        if offs == 0 {
            return None;
        }
        let length = self.memory_length(offs);
        if length == 0 {
            return None;
        }
        Some(MemoryHandle(offs, length))
    }

    /// Get a `MemoryHandle` from a `Val` reference - this can be used to convert a host function's
    /// argument directly to `MemoryHandle`
    pub fn memory_handle_val(&mut self, offs: &Val) -> Option<MemoryHandle> {
        let offs = offs.i64()? as u64;
        let length = self.memory_length(offs);
        if length == 0 {
            return None;
        }
        Some(MemoryHandle(offs, length))
    }

    pub(crate) fn memory_length(&mut self, offs: u64) -> u64 {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        linker
            .get(&mut store, "env", "extism_length")
            .unwrap()
            .into_func()
            .unwrap()
            .call(&mut store, &[Val::I64(offs as i64)], output)
            .unwrap();
        let len = output[0].unwrap_i64() as u64;
        trace!("memory_length: {}, {}", offs, len);
        len
    }

    /// Clear the current plugin error
    pub fn clear_error(&mut self) {
        trace!("CurrentPlugin::clear_error");
        let (linker, mut store) = self.linker_and_store();
        if let Some(f) = linker.get(&mut store, "env", "extism_error_set") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(0)], &mut [])
                .unwrap();
        }
    }

    /// Returns true when the error has been set
    pub fn has_error(&mut self) -> bool {
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

    /// Get the current error message
    pub fn get_error(&mut self) -> Option<&str> {
        let (offs, length) = self.get_error_position();
        if offs == 0 {
            return None;
        }

        let data = self.memory_read(MemoryHandle(offs, length));
        let s = std::str::from_utf8(data);
        match s {
            Ok(s) => Some(s),
            Err(_) => None,
        }
    }

    pub(crate) fn get_error_position(&mut self) -> (u64, u64) {
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
        let length = self.memory_length(offs);
        (offs, length)
    }
}

impl Internal for CurrentPlugin {
    fn store(&self) -> &Store<CurrentPlugin> {
        unsafe { &*self.store }
    }

    fn store_mut(&mut self) -> &mut Store<CurrentPlugin> {
        unsafe { &mut *self.store }
    }

    fn linker(&self) -> &Linker<CurrentPlugin> {
        unsafe { &*self.linker }
    }

    fn linker_mut(&mut self) -> &mut Linker<CurrentPlugin> {
        unsafe { &mut *self.linker }
    }

    fn linker_and_store(&mut self) -> (&mut Linker<CurrentPlugin>, &mut Store<CurrentPlugin>) {
        unsafe { (&mut *self.linker, &mut *self.store) }
    }
}
