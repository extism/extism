use crate::{Error, Internal};

/// A list of all possible value types in WebAssembly.
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
#[repr(C)]
pub enum ValType {
    // NB: the ordering here is intended to match the ordering in
    // `wasmtime_types::WasmType` to help improve codegen when converting.
    /// Signed 32 bit integer.
    I32,
    /// Signed 64 bit integer.
    I64,
    /// Floating point 32 bit integer.
    F32,
    /// Floating point 64 bit integer.
    F64,
    /// A 128 bit number.
    V128,
    /// A reference to a Wasm function.
    FuncRef,
    /// A reference to opaque data in the Wasm instance.
    ExternRef,
}

impl From<wasmtime::ValType> for ValType {
    fn from(value: wasmtime::ValType) -> Self {
        use wasmtime::ValType::*;
        match value {
            I32 => ValType::I32,
            I64 => ValType::I64,
            F32 => ValType::F32,
            F64 => ValType::F64,
            V128 => ValType::V128,
            FuncRef => ValType::FuncRef,
            ExternRef => ValType::ExternRef,
        }
    }
}

impl From<ValType> for wasmtime::ValType {
    fn from(value: ValType) -> Self {
        use ValType::*;
        match value {
            I32 => wasmtime::ValType::I32,
            I64 => wasmtime::ValType::I64,
            F32 => wasmtime::ValType::F32,
            F64 => wasmtime::ValType::F64,
            V128 => wasmtime::ValType::V128,
            FuncRef => wasmtime::ValType::FuncRef,
            ExternRef => wasmtime::ValType::ExternRef,
        }
    }
}

pub struct UserData {
    ptr: *mut std::ffi::c_void,
    free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
}

impl UserData {
    pub fn new(
        ptr: *mut std::ffi::c_void,
        free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
    ) -> Self {
        UserData { ptr, free }
    }

    pub fn as_ptr(&self) -> *mut std::ffi::c_void {
        self.ptr
    }
}

impl Default for UserData {
    fn default() -> Self {
        UserData {
            ptr: std::ptr::null_mut(),
            free: None,
        }
    }
}

impl Drop for UserData {
    fn drop(&mut self) {
        if self.ptr.is_null() {
            return;
        }

        if let Some(free) = self.free {
            free(self.ptr);
        }

        self.ptr = std::ptr::null_mut();
    }
}

unsafe impl Send for UserData {}
unsafe impl Sync for UserData {}

#[allow(clippy::type_complexity)]
pub struct Function(
    pub(crate) String,
    pub(crate) wasmtime::FuncType,
    pub(crate)  std::sync::Arc<
        dyn Fn(
                wasmtime::Caller<Internal>,
                &[wasmtime::Val],
                &mut [wasmtime::Val],
            ) -> Result<(), Error>
            + Sync
            + Send,
    >,
    pub(crate) UserData,
);

impl Function {
    pub fn new<F>(
        name: impl Into<String>,
        args: impl IntoIterator<Item = ValType>,
        returns: impl IntoIterator<Item = ValType>,
        f: F,
    ) -> Function
    where
        F: 'static
            + Fn(
                wasmtime::Caller<Internal>,
                &[wasmtime::Val],
                &mut [wasmtime::Val],
            ) -> Result<(), Error>
            + Sync
            + Send,
    {
        Function(
            name.into(),
            wasmtime::FuncType::new(
                args.into_iter().map(wasmtime::ValType::from),
                returns.into_iter().map(wasmtime::ValType::from),
            ),
            std::sync::Arc::new(f),
            UserData::default(),
        )
    }

    // Set associated user data pointer
    pub fn with_user_data(mut self, user_data: UserData) -> Self {
        self.3 = user_data;
        self
    }

    pub fn name(&self) -> &str {
        &self.0
    }

    pub fn ty(&self) -> &wasmtime::FuncType {
        &self.1
    }
}
