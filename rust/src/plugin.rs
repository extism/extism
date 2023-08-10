use crate::*;
use std::collections::BTreeMap;

pub struct Plugin {
    plugin: extism_runtime::Plugin,
}

pub struct CancelHandle(*const extism_runtime::sdk::ExtismCancelHandle);

unsafe impl Sync for CancelHandle {}
unsafe impl Send for CancelHandle {}

impl CancelHandle {
    pub fn cancel(&self) -> bool {
        unsafe { bindings::extism_plugin_cancel(self.0) }
    }
}

impl Plugin {
    /// Create a new plugin from the given manifest
    pub fn new_with_manifest(
        manifest: &Manifest,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<Plugin, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::new(data, functions, wasi)
    }

    /// Create a new plugin from a WASM module
    pub fn new(
        data: impl AsRef<[u8]>,
        functions: impl IntoIterator<Item = Function>,
        wasi: bool,
    ) -> Result<Plugin, Error> {
        let plugin = extism_runtime::Plugin::new(data, functions, wasi)?;
        Ok(Plugin { plugin })
    }

    /// Set configuration values
    pub fn set_config(&mut self, config: &BTreeMap<String, Option<String>>) -> Result<(), Error> {
        let encoded = serde_json::to_vec(config)?;
        unsafe {
            bindings::extism_plugin_config(
                &mut self.plugin,
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
    pub fn function_exists(&mut self, name: impl AsRef<str>) -> bool {
        self.plugin.function_exists(name)
    }

    /// Return a new `CancelHandle`, which can be used to cancel a running Plugin
    pub fn cancel_handle(&self) -> CancelHandle {
        let ptr = unsafe { bindings::extism_plugin_cancel_handle(&self.plugin) };
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
        let res = self.plugin.call(name, input);
        match res {
            Ok(x) => f(x),
            Err(e) => Err(e),
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
