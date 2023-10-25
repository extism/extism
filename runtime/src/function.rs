use wasmtime::Caller;

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
#[derive(Debug)]
pub enum UserData<T: Sized> {
    C {
        ptr: *mut std::ffi::c_void,
        free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
    },
    // Ref(UserDataPointer),
    Rust(std::sync::Arc<std::sync::Mutex<T>>),
}

impl<T> Clone for UserData<T> {
    fn clone(&self) -> Self {
        match self {
            UserData::C { ptr, .. } => UserData::C {
                ptr: *ptr,
                free: None,
            },
            // UserData::Ref(ptr) => UserData::Ref(ptr.clone()),
            UserData::Rust(data) => UserData::Rust(data.clone()),
        }
    }
}

impl<T> UserData<T> {
    /// Create a new `UserData` from an existing pointer and free function, this is used
    /// by the C API to wrap C pointers into user data
    pub(crate) fn new_pointer(
        ptr: *mut std::ffi::c_void,
        free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
    ) -> Self {
        UserData::C { ptr, free }
    }

    pub(crate) fn as_ptr(&self) -> *mut std::ffi::c_void {
        match self {
            UserData::C { ptr, .. } => *ptr,
            _ => {
                log::error!("Rust UserData cannot be used by C");
                std::ptr::null_mut()
            }
        }
    }

    /// Create a new `UserData` with any Rust type
    pub fn new(x: T) -> Self {
        let data = std::sync::Arc::new(std::sync::Mutex::new(x));
        UserData::Rust(data)
    }

    /// Get a reference to the value stored in `UserData` if it matches the initial type    
    pub fn get(&self) -> Option<std::sync::Arc<std::sync::Mutex<T>>> {
        match self {
            UserData::C { .. } => None,
            // UserData::Ref(UserDataPointer { ptr, .. }) => {
            //     let ptr = unsafe { &*(*ptr as *mut UserData<T>) };
            //     ptr.get()
            // }
            UserData::Rust(data) => Some(data.clone()),
        }
    }
}

impl<T> Default for UserData<T> {
    fn default() -> Self {
        UserData::C {
            ptr: std::ptr::null_mut(),
            free: None,
        }
    }
}

impl<T> Drop for UserData<T> {
    fn drop(&mut self) {
        match self {
            UserData::C { ptr, free } => {
                if let Some(free) = free {
                    free(*ptr);
                }
            }
            _ => (),
        }
    }
}

unsafe impl<T> Send for UserData<T> {}
unsafe impl<T> Sync for UserData<T> {}

type FunctionInner = dyn Fn(wasmtime::Caller<CurrentPlugin>, &[wasmtime::Val], &mut [wasmtime::Val]) -> Result<(), Error>
    + Sync
    + Send;

/// Wraps raw host functions with some additional metadata and user data
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
    pub(crate) _user_data: std::sync::Arc<std::sync::Mutex<dyn std::any::Any>>,
}

impl Clone for Function {
    fn clone(&self) -> Self {
        Function {
            name: self.name.clone(),
            namespace: self.namespace.clone(),
            ty: self.ty.clone(),
            f: self.f.clone(),
            _user_data: self._user_data.clone(),
        }
    }
}

impl Function {
    /// Create a new host function
    pub fn new<T: 'static, F>(
        name: impl Into<String>,
        args: impl IntoIterator<Item = ValType>,
        returns: impl IntoIterator<Item = ValType>,
        user_data: Option<UserData<T>>,
        f: F,
    ) -> Function
    where
        F: 'static
            + Fn(&mut CurrentPlugin, &[Val], &mut [Val], UserData<T>) -> Result<(), Error>
            + Sync
            + Send,
    {
        let user_data = user_data.unwrap_or_default();
        let data = user_data.clone();
        Function {
            name: name.into(),
            ty: wasmtime::FuncType::new(
                args.into_iter().map(wasmtime::ValType::from),
                returns.into_iter().map(wasmtime::ValType::from),
            ),
            f: std::sync::Arc::new(
                move |mut caller: Caller<_>, inp: &[Val], outp: &mut [Val]| {
                    let x = data.clone();
                    f(caller.data_mut(), inp, outp, x)
                },
            ),
            namespace: None,
            _user_data: user_data
                .get()
                .expect("Function::new should not be called with C-created user-data"),
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
/// extism::host_fn!(add_newline(_user_data: (), a: String) -> String { Ok(a + "\n") });
/// ```
/// A few things worth noting:
/// - The function always returns a `Result` that wraps the specified return type
/// - If a first parameter and type are passed (`_user_data` above) followed by a semicolon it will be
///    the name of the `UserData` parameter and can be used from inside the function
//     definition.
#[macro_export]
macro_rules! host_fn {
    ($name: ident  ($($arg:ident : $argty:ty),*) $(-> $ret:ty)? $b:block) => {
        $crate::host_fn!($name (user_data: (); $($arg : $argty),*) $(-> $ret)? {$b});
    };
    ($name: ident  ($user_data:ident : $dataty:ty; $($arg:ident : $argty:ty),*) $(-> $ret:ty)? $b:block) => {
        fn $name(
            plugin: &mut $crate::CurrentPlugin,
            inputs: &[$crate::Val],
            outputs: &mut [$crate::Val],
            #[allow(unused)]
            mut $user_data: $crate::UserData<$dataty>,
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
            if !outputs.is_empty() {
                outputs[0] = plugin.memory_to_val(output);
            }
            Ok(())
        }
    };
}
