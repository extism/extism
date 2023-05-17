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

pub type Val = wasmtime::Val;

pub struct UserData {
    ptr: *mut std::ffi::c_void,
    free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
    is_any: bool,
}

extern "C" fn free_any(ptr: *mut std::ffi::c_void) {
    let ptr = ptr as *mut dyn std::any::Any;
    unsafe { drop(Box::from_raw(ptr)) }
}

impl UserData {
    pub fn new_pointer(
        ptr: *mut std::ffi::c_void,
        free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
    ) -> Self {
        UserData {
            ptr,
            free,
            is_any: false,
        }
    }

    pub fn new<T: std::any::Any>(x: T) -> Self {
        let ptr = Box::into_raw(Box::new(x)) as *mut _;
        UserData {
            ptr,
            free: Some(free_any),
            is_any: true,
        }
    }

    pub fn is_null(&self) -> bool {
        self.ptr.is_null()
    }

    pub fn as_ptr(&self) -> *mut std::ffi::c_void {
        self.ptr
    }

    pub(crate) fn make_copy(&self) -> Self {
        UserData {
            ptr: self.ptr,
            free: None,
            is_any: self.is_any,
        }
    }

    pub fn any(&self) -> Option<&dyn std::any::Any> {
        if !self.is_any || self.is_null() {
            return None;
        }

        unsafe { Some(&*self.ptr) }
    }

    pub fn any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        if !self.is_any || self.is_null() {
            return None;
        }

        unsafe { Some(&mut *self.ptr) }
    }
}

impl Default for UserData {
    fn default() -> Self {
        UserData {
            ptr: std::ptr::null_mut(),
            free: None,
            is_any: false,
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

type FunctionInner = dyn Fn(wasmtime::Caller<Internal>, &[wasmtime::Val], &mut [wasmtime::Val]) -> Result<(), Error>
    + Sync
    + Send;

#[derive(Clone)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) ty: wasmtime::FuncType,
    pub(crate) f: std::sync::Arc<FunctionInner>,
    pub(crate) namespace: Option<String>,
    pub(crate) _user_data: std::sync::Arc<UserData>,
}

impl Function {
    pub fn new<F>(
        name: impl Into<String>,
        args: impl IntoIterator<Item = ValType>,
        returns: impl IntoIterator<Item = ValType>,
        user_data: Option<UserData>,
        f: F,
    ) -> Function
    where
        F: 'static
            + Fn(&mut Internal, &[Val], &mut [Val], UserData) -> Result<(), Error>
            + Sync
            + Send,
    {
        let user_data = user_data.unwrap_or_default();
        let data = user_data.make_copy();
        Function {
            name: name.into(),
            ty: wasmtime::FuncType::new(
                args.into_iter().map(wasmtime::ValType::from),
                returns.into_iter().map(wasmtime::ValType::from),
            ),
            f: std::sync::Arc::new(move |mut caller, inp, outp| {
                f(caller.data_mut(), inp, outp, data.make_copy())
            }),
            namespace: None,
            _user_data: std::sync::Arc::new(user_data),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn namespace(&self) -> Option<&str> {
        self.namespace.as_deref()
    }

    pub fn set_namespace(&mut self, namespace: impl Into<String>) {
        self.namespace = Some(namespace.into());
    }

    pub fn with_namespace(mut self, namespace: impl Into<String>) -> Self {
        self.set_namespace(namespace);
        self
    }

    pub fn ty(&self) -> &wasmtime::FuncType {
        &self.ty
    }
}
