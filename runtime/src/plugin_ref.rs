use crate::*;

// PluginRef is used to access a plugin from a context-scoped plugin registry
pub struct PluginRef<'a> {
    pub id: PluginIndex,
    plugin: *mut Plugin,
    _t: std::marker::PhantomData<&'a ()>,
}

impl<'a> PluginRef<'a> {
    /// Initialize the plugin for a new call
    pub(crate) fn start_call(self, is_start: bool) -> Self {
        trace!("PluginRef::start_call: {}", self.id,);

        let plugin = unsafe { &mut *self.plugin };
        if is_start {
            if let Err(e) = plugin.reset_store() {
                error!("Call to Plugin::reset_store failed: {e:?}");
            }
        }

        if plugin.instance.is_none() {
            trace!("Plugin::instance is none, instantiating");
            if let Err(e) = plugin.instantiate() {
                error!("Plugin::instantiate failed: {e:?}");
                plugin.error(e, ());
            }
        }

        self
    }

    /// Create a `PluginRef` from a context
    ///
    /// - Reinstantiates the plugin if `should_reinstantiate` is set to `true` and WASI is enabled
    pub fn new(ctx: &'a mut Context, plugin_id: PluginIndex, clear_error: bool) -> Option<Self> {
        trace!("Loading plugin {plugin_id}");

        let plugin = if let Some(plugin) = ctx.plugin(plugin_id) {
            plugin
        } else {
            error!("Plugin does not exist: {plugin_id}");
            return ctx.error(format!("Plugin does not exist: {plugin_id}"), None);
        };

        let plugin = unsafe { &mut *plugin };

        if clear_error {
            trace!("Clearing context error");
            ctx.error = None;
            trace!("Clearing plugin error: {plugin_id}");
            plugin.clear_error();
        }

        Some(PluginRef {
            id: plugin_id,
            plugin,
            _t: std::marker::PhantomData,
        })
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
        trace!("Dropping PluginRef {}", self.id);
    }
}
