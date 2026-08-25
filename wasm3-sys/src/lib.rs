//! Extism's Wasm3 bindings.
//!
//! Compiles [Wasm3 v0.9.0](https://github.com/wasm3/wasm3/releases/tag/v0.9.0) from
//! the in-tree `wasm3/` C sources. Do not use the crates.io `wasm3` crate.

use std::ffi::{CStr, CString};
use std::os::raw::c_void;
use std::ptr;
use std::sync::Arc;

pub mod ffi;

/// Upstream version string baked at compile time (`VERSION`).
pub const WASM3_VERSION: &str = env!("WASM3_VERSION");

#[derive(Debug, Clone)]
pub struct Error(String);

impl Error {
    fn from_ptr(p: ffi::M3Result) -> Self {
        if p.is_null() {
            return Error("unknown wasm3 error".into());
        }
        let msg = unsafe { CStr::from_ptr(p) }.to_string_lossy().into_owned();
        Error(msg)
    }

    fn from_runtime(runtime: ffi::IM3Runtime, result: ffi::M3Result) -> Self {
        let mut info = ffi::M3ErrorInfo {
            result,
            runtime: ptr::null_mut(),
            module: ptr::null_mut(),
            function: ptr::null_mut(),
            file: ptr::null(),
            line: 0,
            message: ptr::null(),
        };
        unsafe { ffi::m3_GetErrorInfo(runtime, &mut info) };
        let extra = if info.message.is_null() {
            None
        } else {
            Some(
                unsafe { CStr::from_ptr(info.message) }
                    .to_string_lossy()
                    .into_owned(),
            )
        };
        let mut err = Error::from_ptr(result);
        if let Some(extra) = extra {
            if !extra.is_empty() && extra != err.0 {
                err.0 = format!("{}: {}", err.0, extra);
            }
        }
        err
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for Error {}

pub type Result<T, E = Error> = std::result::Result<T, E>;

fn check(result: ffi::M3Result) -> Result<()> {
    if result.is_null() {
        Ok(())
    } else {
        Err(Error::from_ptr(result))
    }
}

fn check_rt(runtime: ffi::IM3Runtime, result: ffi::M3Result) -> Result<()> {
    if result.is_null() {
        Ok(())
    } else {
        Err(Error::from_runtime(runtime, result))
    }
}

struct EnvInner {
    raw: ffi::IM3Environment,
}

unsafe impl Send for EnvInner {}
unsafe impl Sync for EnvInner {}

impl Drop for EnvInner {
    fn drop(&mut self) {
        if !self.raw.is_null() {
            unsafe { ffi::m3_FreeEnvironment(self.raw) };
        }
    }
}

/// One environment can create many [`Runtime`]s.
#[derive(Clone)]
pub struct Environment {
    inner: Arc<EnvInner>,
}

impl Environment {
    pub fn new() -> Result<Self> {
        let raw = unsafe { ffi::m3_NewEnvironment() };
        if raw.is_null() {
            return Err(Error("m3_NewEnvironment failed".into()));
        }
        Ok(Self {
            inner: Arc::new(EnvInner { raw }),
        })
    }

    fn ptr(&self) -> ffi::IM3Environment {
        self.inner.raw
    }
}

/// Execution context. Owns loaded modules and their Wasm bytes (Wasm3 requires
/// the binary to stay alive for the lifetime of the module).
pub struct Runtime {
    env: Environment,
    raw: ffi::IM3Runtime,
    // Parsed modules need the original bytes to remain valid.
    kept_bytes: Vec<Vec<u8>>,
    // m3_SetModuleName stores a borrowed pointer; keep the CStrings alive.
    kept_names: Vec<CString>,
}

impl Runtime {
    /// Wasm3 operand stack, in bytes. 1 MiB is enough for typical Extism plugins.
    pub const DEFAULT_STACK: u32 = 1024 * 1024;

    pub fn new(env: &Environment, stack_size_bytes: u32) -> Result<Self> {
        let raw = unsafe { ffi::m3_NewRuntime(env.ptr(), stack_size_bytes, ptr::null_mut()) };
        if raw.is_null() {
            return Err(Error("m3_NewRuntime failed".into()));
        }
        Ok(Self {
            env: env.clone(),
            raw,
            kept_bytes: Vec::new(),
            kept_names: Vec::new(),
        })
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }

    /// Parse and load a Wasm module into this runtime.
    pub fn parse_and_load(&mut self, wasm: &[u8]) -> Result<LoadedModule<'_>> {
        if wasm.len() > u32::MAX as usize {
            return Err(Error("wasm module too large".into()));
        }
        self.kept_bytes.push(wasm.to_vec());
        let bytes = self.kept_bytes.last().expect("just pushed wasm bytes");

        let mut module = ptr::null_mut();
        check(unsafe {
            ffi::m3_ParseModule(
                self.env.ptr(),
                &mut module,
                bytes.as_ptr(),
                bytes.len() as u32,
            )
        })?;

        let load = unsafe { ffi::m3_LoadModule(self.raw, module) };
        if !load.is_null() {
            unsafe { ffi::m3_FreeModule(module) };
            return Err(Error::from_runtime(self.raw, load));
        }

        Ok(LoadedModule {
            runtime: self.raw,
            raw: module,
            kept_names: &mut self.kept_names,
            _marker: std::marker::PhantomData,
        })
    }

    pub fn find_function(&self, name: &str) -> Result<Function<'_>> {
        let cname = CString::new(name).map_err(|_| Error("function name contains NUL".into()))?;
        let mut func = ptr::null_mut();
        check_rt(self.raw, unsafe {
            ffi::m3_FindFunction(&mut func, self.raw, cname.as_ptr())
        })?;
        Ok(Function {
            raw: func,
            runtime: self.raw,
            _marker: std::marker::PhantomData,
        })
    }

    pub fn memory(&self) -> Option<&[u8]> {
        let mut size = 0u32;
        let ptr = unsafe { ffi::m3_GetMemory(self.raw, &mut size, 0) };
        if ptr.is_null() {
            return None;
        }
        Some(unsafe { std::slice::from_raw_parts(ptr, size as usize) })
    }

    pub fn memory_mut(&mut self) -> Option<&mut [u8]> {
        let mut size = 0u32;
        let ptr = unsafe { ffi::m3_GetMemory(self.raw, &mut size, 0) };
        if ptr.is_null() {
            return None;
        }
        Some(unsafe { std::slice::from_raw_parts_mut(ptr, size as usize) })
    }

    pub fn as_ptr(&self) -> ffi::IM3Runtime {
        self.raw
    }
}

impl Drop for Runtime {
    fn drop(&mut self) {
        if !self.raw.is_null() {
            unsafe { ffi::m3_FreeRuntime(self.raw) };
            self.raw = ptr::null_mut();
        }
    }
}

/// A module loaded into a [`Runtime`]. Linking host functions must happen on
/// this object *after* load and *before* the first call that needs them —
/// Wasm3 compiles imports at first use, so linking immediately after load is
/// the safe pattern.
pub struct LoadedModule<'rt> {
    runtime: ffi::IM3Runtime,
    raw: ffi::IM3Module,
    kept_names: &'rt mut Vec<CString>,
    _marker: std::marker::PhantomData<&'rt Runtime>,
}

impl LoadedModule<'_> {
    pub fn link_raw(
        &mut self,
        module: &str,
        name: &str,
        signature: &str,
        func: ffi::M3RawCall,
    ) -> Result<()> {
        let module = CString::new(module).map_err(|_| Error("module name contains NUL".into()))?;
        let name = CString::new(name).map_err(|_| Error("function name contains NUL".into()))?;
        let signature =
            CString::new(signature).map_err(|_| Error("signature contains NUL".into()))?;
        check_rt(self.runtime, unsafe {
            ffi::m3_LinkRawFunction(
                self.raw,
                module.as_ptr(),
                name.as_ptr(),
                signature.as_ptr(),
                func,
            )
        })
    }

    /// # Safety
    /// `userdata` must remain valid and correctly typed for as long as the
    /// module can invoke `func`.
    pub unsafe fn link_raw_ex(
        &mut self,
        module: &str,
        name: &str,
        signature: &str,
        func: ffi::M3RawCall,
        userdata: *const c_void,
    ) -> Result<()> {
        let module = CString::new(module).map_err(|_| Error("module name contains NUL".into()))?;
        let name = CString::new(name).map_err(|_| Error("function name contains NUL".into()))?;
        let signature =
            CString::new(signature).map_err(|_| Error("signature contains NUL".into()))?;
        check_rt(self.runtime, unsafe {
            ffi::m3_LinkRawFunctionEx(
                self.raw,
                module.as_ptr(),
                name.as_ptr(),
                signature.as_ptr(),
                func,
                userdata,
            )
        })
    }

    #[cfg(feature = "wasi")]
    pub fn link_wasi(&mut self) -> Result<()> {
        check_rt(self.runtime, unsafe { ffi::m3_LinkWASI(self.raw) })
    }

    /// Register this module under a Manifest-style name (`main`, `extism:host/user`,
    /// `commander`, …). Wasm3 v0.9.0 resolves function imports against this name
    /// when no host function was linked (`ResolveImportedFunction`).
    ///
    /// The C API keeps a borrowed pointer; the string is stored on the [`Runtime`].
    /// Compile this module ([`Self::compile`]) after naming and host-linking it,
    /// and before compiling importers — Wasm3 has one compilation workspace per
    /// runtime, so compiling a callee from inside an importer clobbers the caller.
    pub fn set_name(&mut self, name: &str) -> Result<()> {
        let cname = CString::new(name).map_err(|_| Error("module name contains NUL".into()))?;
        self.kept_names.push(cname);
        let ptr = self
            .kept_names
            .last()
            .expect("just pushed module name")
            .as_ptr();
        unsafe { ffi::m3_SetModuleName(self.raw, ptr) };
        Ok(())
    }

    /// Compile every function in this module. Call this on helpers after
    /// [`Self::set_name`] / host linking and before loading the importer.
    pub fn compile(&mut self) -> Result<()> {
        check_rt(self.runtime, unsafe { ffi::m3_CompileModule(self.raw) })
    }

    pub fn as_ptr(&self) -> ffi::IM3Module {
        self.raw
    }
}

pub struct Function<'rt> {
    raw: ffi::IM3Function,
    runtime: ffi::IM3Runtime,
    _marker: std::marker::PhantomData<&'rt Runtime>,
}

impl Function<'_> {
    pub fn arg_count(&self) -> u32 {
        unsafe { ffi::m3_GetArgCount(self.raw) }
    }

    pub fn ret_count(&self) -> u32 {
        unsafe { ffi::m3_GetRetCount(self.raw) }
    }

    pub fn call_i32(&self, args: &[i32]) -> Result<i32> {
        let ptrs: Vec<*const c_void> = args
            .iter()
            .map(|a| a as *const i32 as *const c_void)
            .collect();
        check_rt(self.runtime, unsafe {
            ffi::m3_Call(self.raw, ptrs.len() as u32, ptrs.as_ptr())
        })?;
        let mut out = 0i32;
        let out_ptr = &mut out as *mut i32 as *mut c_void;
        check_rt(self.runtime, unsafe {
            ffi::m3_GetResults(self.raw, 1, &out_ptr)
        })?;
        Ok(out)
    }

    pub fn call_i64(&self, args: &[i64]) -> Result<i64> {
        let ptrs: Vec<*const c_void> = args
            .iter()
            .map(|a| a as *const i64 as *const c_void)
            .collect();
        check_rt(self.runtime, unsafe {
            ffi::m3_Call(self.raw, ptrs.len() as u32, ptrs.as_ptr())
        })?;
        let mut out = 0i64;
        let out_ptr = &mut out as *mut i64 as *mut c_void;
        check_rt(self.runtime, unsafe {
            ffi::m3_GetResults(self.raw, 1, &out_ptr)
        })?;
        Ok(out)
    }

    /// Call with no arguments and no results (typical Extism export).
    pub fn call_void(&self) -> Result<()> {
        check_rt(self.runtime, unsafe {
            ffi::m3_Call(self.raw, 0, ptr::null())
        })
    }

    pub fn as_ptr(&self) -> ffi::IM3Function {
        self.raw
    }
}

/// Read an i64 argument from a raw-call stack. Result slots come first.
///
/// # Safety
/// `sp` must point at the Wasm3 raw-call stack for a call with at least
/// `ret_count + arg_index + 1` slots.
pub unsafe fn raw_arg_i64(sp: *mut u64, ret_count: usize, arg_index: usize) -> i64 {
    *sp.add(ret_count + arg_index) as i64
}

/// # Safety
/// See [`raw_arg_i64`].
pub unsafe fn raw_arg_i32(sp: *mut u64, ret_count: usize, arg_index: usize) -> i32 {
    *sp.add(ret_count + arg_index) as i32
}

/// Write an i64 into the first result slot.
///
/// # Safety
/// `sp` must point at a writable result slot.
pub unsafe fn raw_set_i64(sp: *mut u64, value: i64) {
    *sp = value as u64;
}

/// # Safety
/// See [`raw_set_i64`].
pub unsafe fn raw_set_i32(sp: *mut u64, value: i32) {
    *sp = value as u64;
}

unsafe impl Send for Environment {}
unsafe impl Sync for Environment {}

// Runtime is not Sync: Wasm3 execution is not re-entrant on one runtime.
unsafe impl Send for Runtime {}
