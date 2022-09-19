use crate::*;

pub struct Plugin<'a> {
    id: bindings::ExtismPlugin,
    context: &'a Context,
}

impl<'a> Plugin<'a> {
    pub fn new_with_manifest(
        ctx: &'a Context,
        manifest: &Manifest,
        wasi: bool,
    ) -> Result<Plugin<'a>, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::new(ctx, data, wasi)
    }

    pub fn new(ctx: &'a Context, data: impl AsRef<[u8]>, wasi: bool) -> Result<Plugin, Error> {
        let plugin = unsafe {
            bindings::extism_plugin_new(
                ctx.pointer,
                data.as_ref().as_ptr(),
                data.as_ref().len() as u64,
                wasi,
            )
        };

        if plugin < 0 {
            let err = unsafe { bindings::extism_error(ctx.pointer, -1) };
            let buf = unsafe { std::ffi::CStr::from_ptr(err) };
            let buf = buf.to_str().unwrap().to_string();
            return Err(Error::UnableToLoadPlugin(buf));
        }

        Ok(Plugin {
            id: plugin,
            context: ctx,
        })
    }

    pub fn update(&mut self, data: impl AsRef<[u8]>, wasi: bool) -> Result<(), Error> {
        let b = unsafe {
            bindings::extism_plugin_update(
                self.context.pointer,
                self.id,
                data.as_ref().as_ptr(),
                data.as_ref().len() as u64,
                wasi,
            )
        };
        if b {
            return Ok(());
        }

        let err = unsafe { bindings::extism_error(self.context.pointer, -1) };
        if !err.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(err) };
            return Err(Error::Message(s.to_str().unwrap().to_string()));
        }

        Err(Error::Message("extism_plugin_update failed".to_string()))
    }

    pub fn update_manifest(&mut self, manifest: &Manifest, wasi: bool) -> Result<(), Error> {
        let data = serde_json::to_vec(manifest)?;
        self.update(data, wasi)
    }

    pub fn set_config(&self, config: &BTreeMap<String, String>) -> Result<(), Error> {
        let encoded = serde_json::to_vec(config)?;
        unsafe {
            bindings::extism_plugin_config(
                self.context.pointer,
                self.id,
                encoded.as_ptr() as *const _,
                encoded.len() as u64,
            )
        };
        Ok(())
    }

    pub fn with_config(self, config: &BTreeMap<String, String>) -> Result<Self, Error> {
        self.set_config(config)?;
        Ok(self)
    }

    pub fn has_function(&self, name: impl AsRef<str>) -> bool {
        let name = std::ffi::CString::new(name.as_ref()).expect("Invalid function name");
        unsafe {
            bindings::extism_plugin_function_exists(
                self.context.pointer,
                self.id,
                name.as_ptr() as *const _,
            )
        }
    }

    pub fn call(&self, name: impl AsRef<str>, input: impl AsRef<[u8]>) -> Result<&[u8], Error> {
        let name = std::ffi::CString::new(name.as_ref()).expect("Invalid function name");
        let rc = unsafe {
            bindings::extism_plugin_call(
                self.context.pointer,
                self.id,
                name.as_ptr() as *const _,
                input.as_ref().as_ptr() as *const _,
                input.as_ref().len() as u64,
            )
        };

        if rc != 0 {
            let err = unsafe { bindings::extism_error(self.context.pointer, self.id) };
            if !err.is_null() {
                let s = unsafe { std::ffi::CStr::from_ptr(err) };
                return Err(Error::Message(s.to_str().unwrap().to_string()));
            }

            return Err(Error::Message("extism_call failed".to_string()));
        }

        let out_len =
            unsafe { bindings::extism_plugin_output_length(self.context.pointer, self.id) };
        unsafe {
            let ptr = bindings::extism_plugin_output_data(self.context.pointer, self.id);
            Ok(std::slice::from_raw_parts(ptr, out_len as usize))
        }
    }
}

impl<'a> Drop for Plugin<'a> {
    fn drop(&mut self) {
        if self.context.pointer.is_null() {
            return;
        }
        unsafe { bindings::extism_plugin_free(self.context.pointer, self.id) }
    }
}
