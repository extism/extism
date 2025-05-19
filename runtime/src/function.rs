use std::sync::Arc;
use wasmtime::Caller;

use crate::{error, trace, CurrentPlugin, Error};

/// An enumeration of all possible value types in WebAssembly.
/// cbindgen:prefix-with-name
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

/// A wrapper around `ValType::I64` to specify arguments that are pointers to memory blocks
pub const PTR: ValType = ValType::I64;

impl From<wasmtime::ValType> for ValType {
    fn from(value: wasmtime::ValType) -> Self {
        use wasmtime::ValType::*;
        match value {
            I32 => ValType::I32,
            I64 => ValType::I64,
            F32 => ValType::F32,
            F64 => ValType::F64,
            V128 => ValType::V128,
            Ref(t) => {
                if t.heap_type().is_func() {
                    ValType::FuncRef
                } else {
                    ValType::ExternRef
                }
            }
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
            FuncRef => wasmtime::ValType::FUNCREF,
            ExternRef => wasmtime::ValType::EXTERNREF,
        }
    }
}

/// Raw WebAssembly values
pub type Val = wasmtime::Val;

/// A pointer to C userdata
#[derive(Debug)]
pub struct CPtr {
    ptr: *mut std::ffi::c_void,
    free: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
}

/// UserDataHandle is an untyped version of `UserData` that is stored inside `Function` to keep a live reference.
#[derive(Clone)]
pub(crate) enum UserDataHandle {
    #[allow(dead_code)]
    C(Arc<CPtr>),
    #[allow(dead_code)]
    Rust(Arc<std::sync::Mutex<dyn std::any::Any>>),
}

/// UserData is used to store additional data that gets passed into host function callbacks
///
/// `UserData` is used to store `C` and `Rust` data from hosts. The Rust data in wrapped in an `Arc<Mutex<T>>` and can be accessed
/// using `UserData::get`. The `C` data is stored as a pointer and cleanup function and isn't usable from Rust. The cleanup function
/// will be called when the inner `CPtr` is dropped.
#[derive(Debug)]
pub enum UserData<T: Sized> {
    C(Arc<CPtr>),
    Rust(Arc<std::sync::Mutex<T>>),
}

impl<T: Default> Default for UserData<T> {
    fn default() -> Self {
        UserData::new(T::default())
    }
}

impl<T> Clone for UserData<T> {
    fn clone(&self) -> Self {
        match self {
            UserData::C(ptr) => UserData::C(ptr.clone()),
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
        UserData::C(Arc::new(CPtr { ptr, free }))
    }

    /// Access the underlying C pointer
    pub(crate) fn as_ptr(&self) -> *mut std::ffi::c_void {
        match self {
            UserData::C(ptr) => ptr.ptr,
            _ => {
                error!("Rust UserData cannot be used by C");
                std::ptr::null_mut()
            }
        }
    }

    /// Create a new `UserData` from a Rust value
    ///
    /// This will wrap the provided value in a reference-counted mutex
    pub fn new(x: T) -> Self {
        let data = Arc::new(std::sync::Mutex::new(x));
        UserData::Rust(data)
    }

    /// Get a copy of the inner value
    pub fn get(&self) -> Result<Arc<std::sync::Mutex<T>>, Error> {
        match self {
            UserData::C { .. } => anyhow::bail!("C UserData should not be used from Rust"),
            UserData::Rust(data) => Ok(data.clone()),
        }
    }
}

impl Drop for CPtr {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            if let Some(free_data) = &self.free {
                free_data(self.ptr);
                self.ptr = std::ptr::null_mut();
            }
        }
    }
}

unsafe impl<T> Send for UserData<T> {}
unsafe impl<T> Sync for UserData<T> {}
unsafe impl Send for CPtr {}
unsafe impl Sync for CPtr {}

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

    pub(crate) params: Vec<ValType>,
    pub(crate) results: Vec<ValType>,

    /// Function handle
    pub(crate) f: Arc<FunctionInner>,

    /// UserData
    pub(crate) _user_data: UserDataHandle,
}

impl Function {
    /// Create a new host function
    pub fn new<T: 'static, F>(
        name: impl Into<String>,
        params: impl IntoIterator<Item = ValType>,
        results: impl IntoIterator<Item = ValType>,
        user_data: UserData<T>,
        f: F,
    ) -> Function
    where
        F: 'static
            + Fn(&mut CurrentPlugin, &[Val], &mut [Val], UserData<T>) -> Result<(), Error>
            + Sync
            + Send,
    {
        let data = user_data.clone();
        let name = name.into();
        let params = params.into_iter().collect();
        let results = results.into_iter().collect();
        trace!("Creating function {name}: params={params:?}, results={results:?}");
        Function {
            name,
            params,
            results,
            f: Arc::new(
                move |mut caller: Caller<_>, inp: &[Val], outp: &mut [Val]| {
                    let x = data.clone();
                    f(caller.data_mut(), inp, outp, x)
                },
            ),
            namespace: None,
            _user_data: match &user_data {
                UserData::C(ptr) => UserDataHandle::C(ptr.clone()),
                UserData::Rust(x) => UserDataHandle::Rust(x.clone()),
            },
        }
    }

    pub(crate) fn ty(&self, engine: &wasmtime::Engine) -> wasmtime::FuncType {
        wasmtime::FuncType::new(
            engine,
            self.params
                .iter()
                .cloned()
                .map(wasmtime::ValType::from)
                .collect::<Vec<_>>(),
            self.results
                .iter()
                .cloned()
                .map(wasmtime::ValType::from)
                .collect::<Vec<_>>(),
        )
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
        let ns = namespace.into();
        trace!("Setting namespace for {} to {ns}", self.name);
        self.namespace = Some(ns);
    }

    /// Update host function module name
    pub fn with_namespace(mut self, namespace: impl Into<String>) -> Self {
        self.set_namespace(namespace);
        self
    }

    /// Get param types
    pub fn params(&self) -> &[ValType] {
        &self.params
    }

    /// Get result types
    pub fn results(&self) -> &[ValType] {
        &self.results
    }
}

/// The `host_fn` macro is used to define typed host functions
///
/// For example, the following defines a host function named `add_newline` that takes a
/// string parameter and returns a string result:
/// ```rust
/// extism::host_fn!(add_newline(_user_data: (); a: String) -> String { Ok(a + "\n") });
/// ```
/// A few things worth noting:
/// - The function always returns a `Result` that wraps the specified return type
/// - If a first parameter and type are passed (`_user_data` above) followed by a semicolon it will be
///   the name of the `UserData` parameter and can be used from inside the function
//    definition.
#[macro_export]
macro_rules! host_fn {
    ($pub:vis $name: ident  ($($arg:ident : $argty:ty),*) $(-> $ret:ty)? $b:block) => {
       $crate::host_fn!($pub $name (user_data: (); $($arg : $argty),*) $(-> $ret)? {$b});
    };
    ($pub:vis $name: ident  ($user_data:ident : $dataty:ty; $($arg:ident : $argty:ty),*) $(-> $ret:ty)? $b:block) => {
        $pub fn $name(
            plugin: &mut $crate::CurrentPlugin,
            inputs: &[$crate::Val],
            outputs: &mut [$crate::Val],
            #[allow(unused)]
            mut $user_data: $crate::UserData<$dataty>,
        ) -> Result<(), $crate::Error> {
            let output = {
                let mut index = 0;
                $(
                    let $arg: $argty = plugin.memory_get_val(&inputs[index])?;
                    #[allow(unused_assignments)]
                    {
                        index += 1;
                    }
                )*
                move || -> Result<_, $crate::Error> { $b }
            };
            let output = output()?;
            let output: $crate::convert::MemoryHandle = plugin.memory_new(&output)?;
            if !outputs.is_empty() {
                outputs[0] = plugin.memory_to_val(output);
            }
            Ok(())
        }
    };
}
