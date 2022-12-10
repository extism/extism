use crate::*;
use std::collections::BTreeMap;

pub struct Plugin<'a> {
    id: extism_runtime::PluginIndex,
    context: &'a Context,
}

impl<'a> Plugin<'a> {
    /// Create plugin from a known-good ID
    ///
    /// # Safety
    /// This function does not check to ensure the provided ID is valid
    pub unsafe fn from_id(id: i32, context: &'a Context) -> Plugin<'a> {
        Plugin { id, context }
    }

    pub fn as_i32(&self) -> i32 {
        self.id
    }

    /// Create a new plugin from the given manifest
    pub fn new_with_manifest(
        ctx: &'a Context,
        manifest: &Manifest,
        wasi: bool,
    ) -> Result<Plugin<'a>, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::new(ctx, data, wasi)
    }

    /// Create a new plugin from the given manifest and import functions
    pub fn new_with_manifest_and_functions(
        ctx: &'a Context,
        manifest: &Manifest,
        imports: impl IntoIterator<Item = &'static extism_runtime::Function>,
        wasi: bool,
    ) -> Result<Plugin<'a>, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::new_with_functions(ctx, data, imports, wasi)
    }

    /// Create a new plugin from a WASM module
    pub fn new(ctx: &'a Context, data: impl AsRef<[u8]>, wasi: bool) -> Result<Plugin, Error> {
        let plugin = ctx.lock().new_plugin(data, wasi);

        if plugin < 0 {
            let err = unsafe { bindings::extism_error(&mut *ctx.lock(), -1) };
            let buf = unsafe { std::ffi::CStr::from_ptr(err) };
            let buf = buf.to_str().unwrap().to_string();
            return Err(Error::UnableToLoadPlugin(buf));
        }

        Ok(Plugin {
            id: plugin,
            context: ctx,
        })
    }

    /// Create a new plugin from a WASM module with imported functions
    pub fn new_with_functions(
        ctx: &'a Context,
        data: impl AsRef<[u8]>,
        imports: impl IntoIterator<Item = &'static Function>,
        wasi: bool,
    ) -> Result<Plugin, Error> {
        let plugin = ctx.lock().new_plugin_with_functions(data, imports, wasi);

        if plugin < 0 {
            let err = unsafe { bindings::extism_error(&mut *ctx.lock(), -1) };
            let buf = unsafe { std::ffi::CStr::from_ptr(err) };
            let buf = buf.to_str().unwrap().to_string();
            return Err(Error::UnableToLoadPlugin(buf));
        }

        Ok(Plugin {
            id: plugin,
            context: ctx,
        })
    }

    /// Update a plugin with the given WASM module
    pub fn update(&mut self, data: impl AsRef<[u8]>, wasi: bool) -> Result<(), Error> {
        let b = unsafe {
            bindings::extism_plugin_update(
                &mut *self.context.lock(),
                self.id,
                data.as_ref().as_ptr(),
                data.as_ref().len() as u64,
                wasi,
            )
        };
        if b {
            return Ok(());
        }

        let err = unsafe { bindings::extism_error(&mut *self.context.lock(), -1) };
        if !err.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(err) };
            return Err(Error::Message(s.to_str().unwrap().to_string()));
        }

        Err(Error::Message("extism_plugin_update failed".to_string()))
    }

    /// Update a plugin with the given manifest
    pub fn update_manifest(&mut self, manifest: &Manifest, wasi: bool) -> Result<(), Error> {
        let data = serde_json::to_vec(manifest)?;
        self.update(data, wasi)
    }

    /// Set configuration values
    pub fn set_config(&mut self, config: &BTreeMap<String, Option<String>>) -> Result<(), Error> {
        let encoded = serde_json::to_vec(config)?;
        unsafe {
            bindings::extism_plugin_config(
                &mut *self.context.lock(),
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
                &mut *self.context.lock(),
                self.id,
                name.as_ptr() as *const _,
            )
        }
    }

    /// Call a function with the given input
    pub fn call(&mut self, name: impl AsRef<str>, input: impl AsRef<[u8]>) -> Result<&[u8], Error> {
        let name = std::ffi::CString::new(name.as_ref()).expect("Invalid function name");
        let rc = unsafe {
            bindings::extism_plugin_call(
                &mut *self.context.lock(),
                self.id,
                name.as_ptr() as *const _,
                input.as_ref().as_ptr() as *const _,
                input.as_ref().len() as u64,
            )
        };

        if rc != 0 {
            let err = unsafe { bindings::extism_error(&mut *self.context.lock(), self.id) };
            if !err.is_null() {
                let s = unsafe { std::ffi::CStr::from_ptr(err) };
                return Err(Error::Message(s.to_str().unwrap().to_string()));
            }

            return Err(Error::Message("extism_call failed".to_string()));
        }

        let out_len =
            unsafe { bindings::extism_plugin_output_length(&mut *self.context.lock(), self.id) };
        unsafe {
            let ptr = bindings::extism_plugin_output_data(&mut *self.context.lock(), self.id);
            Ok(std::slice::from_raw_parts(ptr, out_len as usize))
        }
    }
}

impl<'a> Drop for Plugin<'a> {
    fn drop(&mut self) {
        unsafe { bindings::extism_plugin_free(&mut *self.context.lock(), self.id) }
    }
}
