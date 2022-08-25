use crate::*;

// PluginRef is used to access a plugin from the global plugin registry
pub struct PluginRef<'a> {
    pub plugins: std::sync::MutexGuard<'a, Vec<Plugin>>,
    plugin: *mut Plugin,
}

impl<'a> PluginRef<'a> {
    pub fn init(mut self) -> Self {
        // Initialize
        self.as_mut().clear_error();
        self.as_mut().memory.reset();
        self
    }

    /// # Safety
    ///
    /// This function is used to access the static `PLUGINS` registry
    pub unsafe fn new(plugin: PluginIndex) -> Self {
        let mut plugins = PLUGINS
            .lock()
            .expect("Unable to acquire lock on plugin registry");

        if plugin < 0 || plugin as usize >= plugins.len() {
            panic!("Invalid PluginIndex {plugin}")
        }

        let plugin = plugins.get_unchecked_mut(plugin as usize) as *mut _;

        PluginRef { plugins, plugin }
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
        // Cleanup?
    }
}
