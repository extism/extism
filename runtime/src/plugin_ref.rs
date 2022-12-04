use crate::*;

// PluginRef is used to access a plugin from a context-scoped plugin registry
pub struct PluginRef<'a> {
    pub id: PluginIndex,
    plugin: &'a mut Plugin,
}

impl<'a> PluginRef<'a> {
    /// Initialize the plugin for a new call
    ///
    /// - Resets memory offsets
    /// - Updates `input` pointer
    /// - Reinstantiates if `should_reinstantiate` is set to `true`
    pub fn init(mut self, data: *const u8, data_len: usize) -> Self {
        trace!("PluginRef::init: {}", self.id,);
        self.as_mut().memory.reset();
        self.plugin.set_input(data, data_len);

        // Reinstantiate plugin after calling _start because according to the WASI
        // applicate ABI _start should be called "at most once":
        // https://github.com/WebAssembly/WASI/blob/main/legacy/application-abi.md
        if self.plugin.should_reinstantiate {
            let _ = self.plugin.reinstantiate();
            self.plugin.should_reinstantiate = false;
        }

        self
    }

    /// Create a `PluginRef` from a context
    pub fn new(ctx: &'a mut Context, plugin_id: PluginIndex, clear_error: bool) -> Option<Self> {
        trace!("Loading plugin {plugin_id}");

        if !ctx.plugin_exists(plugin_id) {
            error!("Plugin does not exist: {plugin_id}");
            return ctx.error(format!("Plugin does not exist: {plugin_id}"), None);
        }

        if clear_error {
            trace!("Clearing context error");
            ctx.error = None;
        }

        // `unwrap` is okay here because we already checked with `ctx.plugin_exists` above
        let plugin = ctx.plugin(plugin_id).unwrap();
        if clear_error {
            trace!("Clearing plugin error: {plugin_id}");
            plugin.clear_error();
        }

        Some(PluginRef {
            id: plugin_id,
            plugin,
        })
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
