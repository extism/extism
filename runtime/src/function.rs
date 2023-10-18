use crate::{CurrentPlugin, Error};

/// An enumeration of all possible value types in WebAssembly.
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

/// Raw WebAssembly values
pub type Val = wasmtime::Val;

/// UserData is an opaque pointer used to store additional data
/// that gets passed into host function callbacks
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
    /// Create a new `UserData` from an existing pointer and free function, this is used
    /// by the C API to wrap C pointers into user data
    pub(crate) fn new_pointer(
        ptr: *mut std::ffi::c_void,
        free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
    ) -> Self {
        UserData {
            ptr,
            free,
            is_any: false,
        }
    }

    /// Create a new `UserData` with any Rust type
    pub fn new<T: std::any::Any>(x: T) -> Self {
        let ptr = Box::into_raw(Box::new(x)) as *mut _;
        UserData {
            ptr,
            free: Some(free_any),
            is_any: true,
        }
    }

    /// Returns `true` if the underlying pointer is `null`
    pub fn is_null(&self) -> bool {
        self.ptr.is_null()
    }

    /// Get the user data pointer
    pub(crate) fn as_ptr(&self) -> *mut std::ffi::c_void {
        self.ptr
    }

    pub(crate) fn make_copy(&self) -> Self {
        UserData {
            ptr: self.ptr,
            free: None,
            is_any: self.is_any,
        }
    }

    /// Get the pointer as an `Any` value - this will only return `Some` if `UserData::new` was used to create the value,
    /// when `UserData::new_pointer` is used there is no way to know the original type of the pointer
    pub fn any(&self) -> Option<&dyn std::any::Any> {
        if !self.is_any || self.is_null() {
            return None;
        }

        unsafe { Some(&*self.ptr) }
    }

    /// Get a reference to the value stored in `UserData` if it matches the initial type    
    pub fn get<T: 'static>(&self) -> Option<&T> {
        self.any().and_then(|x| x.downcast_ref())
    }

    /// Get the pointer as a mutable `Any` value - this will only return `Some` if `UserData::new` was used to create the value,
    /// when `UserData::new_pointer` is used there is no way to know the original type of the pointer
    pub fn any_mut(&mut self) -> Option<&mut dyn std::any::Any> {
        if !self.is_any || self.is_null() {
            return None;
        }

        unsafe { Some(&mut *self.ptr) }
    }

    /// Get a mutable reference to the value stored in `UserData` if it matches the initial type    
    pub fn get_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.any_mut().and_then(|x| x.downcast_mut())
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

type FunctionInner = dyn Fn(wasmtime::Caller<CurrentPlugin>, &[wasmtime::Val], &mut [wasmtime::Val]) -> Result<(), Error>
    + Sync
    + Send;

/// Wraps raw host functions with some additional metadata and user data
#[derive(Clone)]
pub struct Function {
    /// Function name
    pub(crate) name: String,

    /// Module name
    pub(crate) namespace: Option<String>,

    /// Function type
    pub(crate) ty: wasmtime::FuncType,

    /// Function handle
    pub(crate) f: std::sync::Arc<FunctionInner>,

    /// UserData
    pub(crate) _user_data: std::sync::Arc<UserData>,
}

impl Function {
    /// Create a new host function
    pub fn new<F>(
        name: impl Into<String>,
        args: impl IntoIterator<Item = ValType>,
        returns: impl IntoIterator<Item = ValType>,
        user_data: Option<UserData>,
        f: F,
    ) -> Function
    where
        F: 'static
            + Fn(&mut CurrentPlugin, &[Val], &mut [Val], UserData) -> Result<(), Error>
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

    /// Host function name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Host function module name
    pub fn namespace(&self) -> Option<&str> {
        self.namespace.as_deref()
    }

    /// Set host function module name
    pub fn set_namespace(&mut self, namespace: impl Into<String>) {
        self.namespace = Some(namespace.into());
    }

    /// Update host function module name
    pub fn with_namespace(mut self, namespace: impl Into<String>) -> Self {
        self.set_namespace(namespace);
        self
    }

    /// Get function type
    pub fn ty(&self) -> &wasmtime::FuncType {
        &self.ty
    }
}

/// The `host_fn` macro is used to define typed host functions
///
/// For example, the following defines a host function named `add_newline` that takes a
/// string parameter and returns a string result:
/// ```rust
/// extism::host_fn!(add_newline(_user_data, a: String) -> String { Ok(a + "\n") });
/// ```
/// A few things worth noting:
/// - The function always returns a `Result` that wraps the specified return type
/// - If an untyped first parameter is passed (`_user_data` above) it will be
///    the name of the `UserData` parameter and can be used from inside the function
//     definition.
#[macro_export]
macro_rules! host_fn {
    ($name: ident  ($($arg:ident : $argty:ty),*) $(-> $ret:ty)? $b:block) => {
        $crate::host_fn!($name (user_data, $($arg : $argty),*) $(-> $ret)? {$b});
    };
    ($name: ident  ($user_data:ident, $($arg:ident : $argty:ty),*) $(-> $ret:ty)? $b:block) => {
        fn $name(
            plugin: &mut $crate::CurrentPlugin,
            inputs: &[$crate::Val],
            outputs: &mut [$crate::Val],
            #[allow(unused)]
            mut $user_data: $crate::UserData,
        ) -> Result<(), $crate::Error> {
            let mut index = 0;
            $(
                let $arg: $argty = plugin.memory_get_val(&inputs[index])?;
                #[allow(unused_assignments)]
                {
                    index += 1;
                }
            )*
            let output = move || -> Result<_, $crate::Error> { $b };
            let output: $crate::convert::MemoryHandle = plugin.memory_new(&output()?)?;
            outputs[0] = plugin.memory_to_val(output);
            Ok(())
        }
    };
}
