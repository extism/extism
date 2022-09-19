use crate::*;

// PluginRef is used to access a plugin from the global plugin registry
pub struct PluginRef<'a> {
    pub id: PluginIndex,
    plugin: &'a mut Plugin,
}

impl<'a> PluginRef<'a> {
    pub fn init(mut self) -> Self {
        trace!("PluginRef::init: {}", self.id,);
        // Initialize
        self.as_mut().memory.reset();
        let internal = self.as_mut().memory.store.data_mut();
        internal.input = std::ptr::null();
        internal.input_length = 0;
        self
    }

    /// # Safety
    ///
    /// This function is used to access the static `PLUGINS` registry
    pub unsafe fn new(ctx: &'a mut Context, plugin_id: PluginIndex, clear_error: bool) -> Self {
        trace!("Loading plugin {plugin_id}");

        if plugin_id < 0 {
            panic!("Invalid PluginIndex in PluginRef::new: {plugin_id}");
        }

        if clear_error {
            ctx.error = None;
        }

        let plugin = ctx.plugin(plugin_id);

        let plugin = match plugin {
            None => {
                panic!("Plugin does not exist: {plugin_id}");
            }
            Some(p) => p,
        };

        if clear_error {
            plugin.clear_error();
        }

        PluginRef {
            id: plugin_id,
            plugin,
        }
    }
}

impl<'a> AsRef<Plugin> for PluginRef<'a> {
    fn as_ref(&self) -> &Plugin {
        self.plugin
    }
}

impl<'a> AsMut<Plugin> for PluginRef<'a> {
    fn as_mut(&mut self) -> &mut Plugin {
        self.plugin
    }
}

impl<'a> Drop for PluginRef<'a> {
    fn drop(&mut self) {
        trace!("Dropping plugin {}", self.id);
        // Cleanup?
    }
}
