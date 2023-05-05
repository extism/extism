use crate::*;

// PluginRef is used to access a plugin from a context-scoped plugin registry
pub struct PluginRef<'a> {
    pub id: PluginIndex,
    pub(crate) epoch_timer_tx: std::sync::mpsc::SyncSender<TimerAction>,
    plugin: *mut Plugin,
    _t: std::marker::PhantomData<&'a ()>,
}

impl<'a> PluginRef<'a> {
    /// Initialize the plugin for a new call
    ///
    /// - Resets memory offsets
    /// - Updates `input` pointer
    pub fn init(mut self, data: *const u8, data_len: usize) -> Self {
        trace!("PluginRef::init: {}", self.id,);
        let plugin = self.as_mut();
        plugin.memory_mut().reset();
        if plugin.has_wasi() || plugin.runtime.is_some() {
            if let Err(e) = plugin.reinstantiate() {
                error!("Failed to reinstantiate: {e:?}");
                plugin
                    .internal()
                    .set_error(format!("Failed to reinstantiate: {e:?}"));
                return self;
            }
        }
        plugin.set_input(data, data_len);
        self
    }

    /// Create a `PluginRef` from a context
    ///
    /// - Reinstantiates the plugin if `should_reinstantiate` is set to `true` and WASI is enabled
    pub fn new(ctx: &'a mut Context, plugin_id: PluginIndex, clear_error: bool) -> Option<Self> {
        trace!("Loading plugin {plugin_id}");

        let epoch_timer_tx = ctx.epoch_timer_tx.clone();

        let plugin = if let Some(plugin) = ctx.plugin(plugin_id) {
            plugin
        } else {
            error!("Plugin does not exist: {plugin_id}");
            return ctx.error(format!("Plugin does not exist: {plugin_id}"), None);
        };

        if clear_error {
            trace!("Clearing context error");
            ctx.error = None;
            trace!("Clearing plugin error: {plugin_id}");
            unsafe {
                (&*plugin).internal().clear_error();
            }
        }

        Some(PluginRef {
            id: plugin_id,
            plugin,
            epoch_timer_tx,
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
        // Cleanup?
    }
}
