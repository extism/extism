use crate::*;

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
    pub(crate) id: uuid::Uuid,
}

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
            if desired >= max {
                return Err(Error::msg("oom"));
            }
        }

        let d = desired - current;
        if d > self.bytes_left {
            return Err(Error::msg("oom"));
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
    /// Get a `MemoryHandle` from a memory offset
    pub fn memory_handle(&mut self, offs: u64) -> Option<MemoryHandle> {
        let len = self.memory_length(offs).unwrap_or_default();
        if len == 0 {
            trace!(
                plugin = self.id.to_string(),
                "memory handle not found: offs = {offs}",
            );
            return None;
        }

        trace!(
            plugin = self.id.to_string(),
            "memory handle found: offs = {offs}, length = {len}",
        );
        Some(MemoryHandle {
            offset: offs,
            length: len,
        })
    }

    /// Access memory bytes as `str`
    pub fn memory_str(&mut self, handle: MemoryHandle) -> Result<&mut str, Error> {
        let bytes = self.memory_bytes(handle)?;
        let s = std::str::from_utf8_mut(bytes)?;
        Ok(s)
    }

    /// Allocate a handle large enough for the encoded Rust type and copy it into Extism memory
    pub fn memory_new<'a, T: ToBytes<'a>>(&mut self, t: T) -> Result<MemoryHandle, Error> {
        let data = t.to_bytes()?;
        let data = data.as_ref();
        let handle = self.memory_alloc(data.len() as u64)?;
        let bytes = self.memory_bytes(handle)?;
        bytes.copy_from_slice(data.as_ref());
        Ok(handle)
    }

    /// Decode a Rust type from Extism memory
    pub fn memory_get<'a, T: FromBytes<'a>>(
        &'a mut self,
        handle: MemoryHandle,
    ) -> Result<T, Error> {
        let data = self.memory_bytes(handle)?;
        T::from_bytes(data)
    }

    /// Decode a Rust type from Extism memory from an offset in memory specified by a `Val`
    pub fn memory_get_val<'a, T: FromBytes<'a>>(&'a mut self, offs: &Val) -> Result<T, Error> {
        trace!(
            plugin = self.id.to_string(),
            "memory_set_val: val = {:?}",
            offs
        );
        if let Some(handle) = self.memory_handle(offs.i64().unwrap_or(0) as u64) {
            let data = self.memory_bytes(handle)?;
            T::from_bytes(data)
        } else {
            anyhow::bail!("{} invalid memory offset: {offs:?}", self.id)
        }
    }

    /// Encode a Rust type into Extism memory and store it in the given `Val`, this can be used to return
    /// values from host functions.    
    pub fn memory_set_val<'a, T: ToBytes<'a>>(
        &'a mut self,
        offs: &mut Val,
        data: T,
    ) -> Result<(), Error> {
        let mem = self.memory_new(data)?;
        trace!(
            plugin = self.id.to_string(),
            "memory_set_val: val = {:?}",
            offs
        );
        *offs = Val::I64(mem.offset as i64);
        Ok(())
    }

    pub fn memory_bytes(&mut self, handle: MemoryHandle) -> Result<&mut [u8], Error> {
        let (linker, mut store) = self.linker_and_store();
        if let Some(mem) = linker.get(&mut store, EXTISM_ENV_MODULE, "memory") {
            let mem = mem.into_memory().unwrap();
            let ptr = unsafe { mem.data_ptr(&store).add(handle.offset() as usize) };
            if ptr.is_null() {
                return Ok(&mut []);
            }
            return Ok(unsafe { std::slice::from_raw_parts_mut(ptr, handle.len()) });
        }

        anyhow::bail!("{} unable to locate extism memory", self.id)
    }

    pub fn memory_alloc(&mut self, n: u64) -> Result<MemoryHandle, Error> {
        if n == 0 {
            return Ok(MemoryHandle {
                offset: 0,
                length: 0,
            });
        }
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        if let Some(f) = linker.get(&mut store, EXTISM_ENV_MODULE, "alloc") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(n as i64)], output)?;
        } else {
            anyhow::bail!("{} unable to allocate memory", self.id);
        }
        let offs = output[0].unwrap_i64() as u64;
        if offs == 0 {
            anyhow::bail!("{} out of memory", self.id)
        }
        trace!(
            plugin = self.id.to_string(),
            "memory_alloc({}) = {}",
            offs,
            n
        );
        Ok(MemoryHandle {
            offset: offs,
            length: n,
        })
    }

    /// Free a block of Extism plugin memory
    pub fn memory_free(&mut self, handle: MemoryHandle) -> Result<(), Error> {
        let (linker, mut store) = self.linker_and_store();
        if let Some(f) = linker.get(&mut store, EXTISM_ENV_MODULE, "free") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(handle.offset as i64)], &mut [])?;
        } else {
            anyhow::bail!("unable to locate an extism kernel function: free",)
        }
        Ok(())
    }

    pub fn memory_length(&mut self, offs: u64) -> Result<u64, Error> {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        if let Some(f) = linker.get(&mut store, EXTISM_ENV_MODULE, "length") {
            f.into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(offs as i64)], output)?;
        } else {
            anyhow::bail!("unable to locate an extism kernel function: length",)
        }
        let len = output[0].unwrap_i64() as u64;
        trace!(
            plugin = self.id.to_string(),
            "memory_length({}) = {}",
            offs,
            len
        );
        Ok(len)
    }

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
        id: uuid::Uuid,
    ) -> Result<Self, Error> {
        let wasi = if wasi {
            let auth = wasmtime_wasi::ambient_authority();
            let mut ctx = wasmtime_wasi::WasiCtxBuilder::new();
            for (k, v) in manifest.config.iter() {
                ctx.env(k, v)?;
            }

            if let Some(a) = &manifest.allowed_paths {
                for (k, v) in a.iter() {
                    let d = wasmtime_wasi::Dir::open_ambient_dir(k, auth)?;
                    ctx.preopened_dir(d, v)?;
                }
            }

            // Enable WASI output, typically used for debugging purposes
            if std::env::var("EXTISM_ENABLE_WASI_OUTPUT").is_ok() {
                ctx.inherit_stdout().inherit_stderr();
            }

            Some(Wasi { ctx: ctx.build() })
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
            id,
        })
    }

    /// Get a pointer to the plugin memory
    pub(crate) fn memory_ptr(&mut self) -> *mut u8 {
        if let Some(mem) = self.memory() {
            let (_, store) = self.linker_and_store();
            return mem.data_ptr(store);
        }

        std::ptr::null_mut()
    }

    /// Get extism memory
    pub(crate) fn memory(&mut self) -> Option<wasmtime::Memory> {
        let (linker, mut store) = self.linker_and_store();
        linker
            .get(&mut store, EXTISM_ENV_MODULE, "memory")?
            .into_memory()
    }

    /// Get a `MemoryHandle` from a `Val` reference - this can be used to convert a host function's
    /// argument directly to `MemoryHandle`
    pub fn memory_from_val(&mut self, offs: &Val) -> Option<MemoryHandle> {
        let offs = offs.i64()? as u64;
        let length = self.memory_length(offs).unwrap_or_default();
        if length == 0 {
            return None;
        }
        Some(MemoryHandle {
            offset: offs,
            length,
        })
    }

    /// Get a `MemoryHandle` from a `Val` reference - this can be used to convert a host function's
    /// argument directly to `MemoryHandle`
    pub fn memory_to_val(&mut self, handle: MemoryHandle) -> Val {
        Val::I64(handle.offset() as i64)
    }

    /// Clear the current plugin error
    pub fn clear_error(&mut self) {
        trace!(plugin = self.id.to_string(), "CurrentPlugin::clear_error");
        let (linker, mut store) = self.linker_and_store();
        if let Some(f) = linker.get(&mut store, EXTISM_ENV_MODULE, "error_set") {
            let res = f
                .into_func()
                .unwrap()
                .call(&mut store, &[Val::I64(0)], &mut []);
            if let Err(e) = res {
                error!(
                    plugin = self.id.to_string(),
                    "unable to clear error: {:?}", e
                );
            }
        }
    }

    /// Get the current error message
    pub fn get_error(&mut self) -> Option<&str> {
        let (offs, length) = self.get_error_position();
        if offs == 0 {
            return None;
        }

        let s = self.memory_str(MemoryHandle {
            offset: offs,
            length,
        });
        match s {
            Ok(s) => Some(s),
            Err(_) => None,
        }
    }

    #[doc(hidden)]
    pub fn set_error(&mut self, s: impl AsRef<str>) -> Result<(u64, u64), Error> {
        let s = s.as_ref();
        debug!(plugin = self.id.to_string(), "set error: {:?}", s);
        let handle = self.current_plugin_mut().memory_new(s)?;
        let (linker, mut store) = self.linker_and_store();
        if let Some(f) = linker.get(&mut store, EXTISM_ENV_MODULE, "error_set") {
            f.into_func().unwrap().call(
                &mut store,
                &[Val::I64(handle.offset() as i64)],
                &mut [],
            )?;
            Ok((handle.offset(), s.len() as u64))
        } else {
            anyhow::bail!("extism:host/env::error_set not found");
        }
    }

    pub(crate) fn get_error_position(&mut self) -> (u64, u64) {
        let (linker, mut store) = self.linker_and_store();
        let output = &mut [Val::I64(0)];
        if let Some(f) = linker.get(&mut store, EXTISM_ENV_MODULE, "error_get") {
            if let Err(e) = f.into_func().unwrap().call(&mut store, &[], output) {
                error!(
                    plugin = self.id.to_string(),
                    "unable to call extism:host/env::error_get: {:?}", e
                );
                return (0, 0);
            }
        };
        let offs = output[0].unwrap_i64() as u64;
        let length = self.memory_length(offs).unwrap_or_default();
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
