use crate::*;

// PluginRef is used to access a plugin from the global plugin registry
pub struct PluginRef<'a> {
    pub id: PluginIndex,
    pub plugins: std::sync::MutexGuard<'a, Vec<Plugin>>,
    plugin: *mut Plugin,
}

impl<'a> PluginRef<'a> {
    pub fn init(mut self) -> Self {
        trace!(
            "Resetting memory and clearing error message for plugin {}",
            self.id,
        );
        // Initialize
        self.as_mut().clear_error();
        self.as_mut().memory.reset();
        let internal = self.as_mut().memory.store.data_mut();
        internal.input = std::ptr::null();
        internal.input_length = 0;
        self
    }

    /// # Safety
    ///
    /// This function is used to access the static `PLUGINS` registry
    pub unsafe fn new(plugin_id: PluginIndex) -> Self {
        let mut plugins = match PLUGINS.lock() {
            Ok(p) => p,
            Err(e) => e.into_inner(),
        };

        trace!("Loading plugin {plugin_id}");

        if plugin_id < 0 || plugin_id as usize >= plugins.len() {
            drop(plugins);
            panic!("Invalid PluginIndex {plugin_id}");
        }

        let plugin = plugins.get_unchecked_mut(plugin_id as usize) as *mut _;

        PluginRef {
            id: plugin_id,
            plugins,
            plugin,
        }
    }
}

impl<'a> AsRef<Plugin> for PluginRef<'a> {
    fn as_ref(&self) -> &Plugin {
        unsafe { &*self.plugin }
    }
}

impl<'a> AsMut<Plugin> for PluginRef<'a> {
    fn as_mut(&mut self) -> &mut Plugin {
        unsafe { &mut *self.plugin }
    }
}

impl<'a> Drop for PluginRef<'a> {
    fn drop(&mut self) {
        trace!("Dropping plugin {}", self.id);
        // Cleanup?
    }
}
