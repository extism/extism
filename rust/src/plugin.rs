use crate::*;
use std::collections::BTreeMap;

enum RefOrOwned<'a, T> {
    Ref(&'a T),
    Owned(T),
}

pub struct Plugin<'a> {
    id: extism_runtime::PluginIndex,
    context: RefOrOwned<'a, Context>,
    functions: Vec<Function>,
}

impl<'a, T> From<&'a T> for RefOrOwned<'a, T> {
    fn from(value: &'a T) -> Self {
        RefOrOwned::Ref(value)
    }
}

impl<'a, T> From<T> for RefOrOwned<'a, T> {
    fn from(value: T) -> Self {
        RefOrOwned::Owned(value)
    }
}

impl<'a, T> AsRef<T> for RefOrOwned<'a, T> {
    fn as_ref(&self) -> &T {
        match self {
            RefOrOwned::Ref(x) => x,
            RefOrOwned::Owned(x) => x,
        }
    }
}

pub struct CancelHandle(pub(crate) *const extism_runtime::sdk::ExtismCancelHandle);

unsafe impl Sync for CancelHandle {}
unsafe impl Send for CancelHandle {}

impl CancelHandle {
    pub fn cancel(&self) -> bool {
        unsafe { extism_runtime::sdk::extism_plugin_cancel(self.0) }
    }
}

impl<'a> Plugin<'a> {
    /// Create plugin from a known-good ID
    ///
    /// # Safety
    /// This function does not check to ensure the provided ID is valid
    pub unsafe fn from_id(id: i32, context: &'a Context) -> Plugin<'a> {
        let context = RefOrOwned::Ref(context);
        Plugin {
            id,
            context,
            functions: vec![],
        }
    }

    pub fn context(&self) -> &Context {
        match &self.context {
            RefOrOwned::Ref(x) => x,
            RefOrOwned::Owned(x) => x,
        }
    }

    pub fn as_i32(&self) -> i32 {
        self.id
    }

    /// Create a new plugin from the given manifest in its own context
    pub fn create_with_manifest(
        manifest: &Manifest,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<Plugin<'a>, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::create(data, functions, wasi)
    }

    /// Create a new plugin from a WASM module in its own context
    pub fn create(
        data: impl AsRef<[u8]>,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<Plugin<'a>, Error> {
        let ctx = Context::new();
        let functions = functions.into_iter().collect();
        let plugin = ctx.lock().new_plugin(data, &functions, wasi);

        if plugin < 0 {
            let err = unsafe { bindings::extism_error(&mut *ctx.lock(), -1) };
            let buf = unsafe { std::ffi::CStr::from_ptr(err) };
            let buf = buf.to_str().unwrap();
            return Err(Error::msg(buf));
        }

        Ok(Plugin {
            id: plugin,
            context: ctx.into(),
            functions,
        })
    }

    /// Create a new plugin from the given manifest
    pub fn new_with_manifest(
        ctx: &'a Context,
        manifest: &Manifest,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<Plugin<'a>, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::new(ctx, data, functions, wasi)
    }

    /// Create a new plugin from a WASM module
    pub fn new(
        ctx: &'a Context,
        data: impl AsRef<[u8]>,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<Plugin<'a>, Error> {
        let functions = functions.into_iter().collect();
        let plugin = ctx.lock().new_plugin(data, &functions, wasi);

        if plugin < 0 {
            let err = unsafe { bindings::extism_error(&mut *ctx.lock(), -1) };
            let buf = unsafe { std::ffi::CStr::from_ptr(err) };
            let buf = buf.to_str().unwrap();
            return Err(Error::msg(buf));
        }

        Ok(Plugin {
            id: plugin,
            context: ctx.into(),
            functions,
        })
    }

    /// Update a plugin with the given manifest
    pub fn update_with_manifest(
        &mut self,
        manifest: &Manifest,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<(), Error> {
        let data = serde_json::to_vec(manifest)?;
        self.update(data, functions, wasi)
    }

    /// Update a plugin with the given WASM module
    pub fn update(
        &mut self,
        data: impl AsRef<[u8]>,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<(), Error> {
        self.functions = functions.into_iter().collect();
        let functions = self
            .functions
            .iter()
            .map(|x| bindings::ExtismFunction::from(x.clone()));
        let mut functions = functions
            .into_iter()
            .map(|x| &x as *const _)
            .collect::<Vec<_>>();
        let b = unsafe {
            bindings::extism_plugin_update(
                &mut *self.context.as_ref().lock(),
                self.id,
                data.as_ref().as_ptr(),
                data.as_ref().len() as u64,
                functions.as_mut_ptr(),
                functions.len() as u64,
                wasi,
            )
        };
        if b {
            return Ok(());
        }

        let err = unsafe { bindings::extism_error(&mut *self.context.as_ref().lock(), -1) };
        if !err.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(err) };
            return Err(Error::msg(s.to_str().unwrap()));
        }

        Err(Error::msg("extism_plugin_update failed"))
    }

    /// Set configuration values
    pub fn set_config(&mut self, config: &BTreeMap<String, Option<String>>) -> Result<(), Error> {
        let encoded = serde_json::to_vec(config)?;
        unsafe {
            bindings::extism_plugin_config(
                &mut *self.context.as_ref().lock(),
                self.id,
                encoded.as_ptr() as *const _,
                encoded.len() as u64,
            )
        };
        Ok(())
    }

    /// Set configuration values, builder-style
    pub fn with_config(mut self, config: &BTreeMap<String, Option<String>>) -> Result<Self, Error> {
        self.set_config(config)?;
        Ok(self)
    }

    /// Returns true if the plugin has a function matching `name`
    pub fn has_function(&self, name: impl AsRef<str>) -> bool {
        let name = std::ffi::CString::new(name.as_ref()).expect("Invalid function name");
        unsafe {
            bindings::extism_plugin_function_exists(
                &mut *self.context.as_ref().lock(),
                self.id,
                name.as_ptr() as *const _,
            )
        }
    }

    pub fn cancel_handle(&self) -> CancelHandle {
        let ptr = unsafe {
            bindings::extism_plugin_cancel_handle(&mut *self.context.as_ref().lock(), self.id)
        };

        CancelHandle(ptr)
    }

    /// Call a function with the given input and call a callback with the output, this should be preferred when
    /// a single plugin may be acessed from multiple threads because the lock on the plugin is held during the
    /// callback, ensuring the output value is protected from modification.
    pub fn call_map<'b, T, F: FnOnce(&'b [u8]) -> Result<T, Error>>(
        &'b mut self,
        name: impl AsRef<str>,
        input: impl AsRef<[u8]>,
        f: F,
    ) -> Result<T, Error> {
        let context = &mut *self.context.as_ref().lock();
        let name = std::ffi::CString::new(name.as_ref()).expect("Invalid function name");
        let rc = unsafe {
            bindings::extism_plugin_call(
                context,
                self.id,
                name.as_ptr() as *const _,
                input.as_ref().as_ptr() as *const _,
                input.as_ref().len() as u64,
            )
        };

        if rc != 0 {
            let err = unsafe { bindings::extism_error(context, self.id) };
            if !err.is_null() {
                let s = unsafe { std::ffi::CStr::from_ptr(err) };
                return Err(Error::msg(s.to_str().unwrap()));
            }

            return Err(Error::msg("extism_call failed"));
        }

        let out_len = unsafe { bindings::extism_plugin_output_length(context, self.id) };
        unsafe {
            let ptr = bindings::extism_plugin_output_data(context, self.id);
            f(std::slice::from_raw_parts(ptr, out_len as usize))
        }
    }

    /// Call a function with the given input
    pub fn call<'b>(
        &'b mut self,
        name: impl AsRef<str>,
        input: impl AsRef<[u8]>,
    ) -> Result<&'b [u8], Error> {
        self.call_map(name, input, |x| Ok(x))
    }
}

impl<'a> Drop for Plugin<'a> {
    fn drop(&mut self) {
        unsafe { bindings::extism_plugin_free(&mut *self.context.as_ref().lock(), self.id) }
    }
}
