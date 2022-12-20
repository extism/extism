use std::cell::UnsafeCell;
use std::collections::{BTreeMap, VecDeque};

use crate::*;

static mut TIMER: std::sync::Mutex<Option<Timer>> = std::sync::Mutex::new(None);

/// A `Context` is used to store and manage plugins
pub struct Context {
    /// Plugin registry
    pub plugins: BTreeMap<PluginIndex, UnsafeCell<Plugin>>,

    /// Error message
    pub error: Option<std::ffi::CString>,
    next_id: std::sync::atomic::AtomicI32,
    reclaimed_ids: VecDeque<PluginIndex>,

    // Timeout thread
    pub(crate) epoch_timer_tx: std::sync::mpsc::SyncSender<TimerAction>,
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

const START_REUSING_IDS: usize = 25;

impl Context {
    pub(crate) fn timer() -> std::sync::MutexGuard<'static, Option<Timer>> {
        match unsafe { TIMER.lock() } {
            Ok(x) => x,
            Err(e) => e.into_inner(),
        }
    }

    /// Create a new context
    pub fn new() -> Context {
        let timer = &mut *Self::timer();

        let tx = match timer {
            None => Timer::init(timer),
            Some(t) => t.tx.clone(),
        };

        Context {
            plugins: BTreeMap::new(),
            error: None,
            next_id: std::sync::atomic::AtomicI32::new(0),
            reclaimed_ids: VecDeque::new(),
            epoch_timer_tx: tx,
        }
    }

    /// Get the next valid plugin ID
    pub fn next_id(&mut self) -> Result<PluginIndex, Error> {
        // Make sure we haven't exhausted all plugin IDs, to reach this it would require the machine
        // running this code to have a lot of memory - no computer I tested on was able to allocate
        // the max number of plugins.
        //
        // Since `Context::remove` collects IDs that have been removed we will
        // try to use one of those before returning an error
        let exhausted = self.next_id.load(std::sync::atomic::Ordering::SeqCst) == PluginIndex::MAX;

        // If there are a significant number of old IDs we can start to re-use them
        if self.reclaimed_ids.len() >= START_REUSING_IDS || exhausted {
            if let Some(x) = self.reclaimed_ids.pop_front() {
                return Ok(x);
            }

            if exhausted {
                return Err(anyhow::format_err!(
                    "All plugin descriptors are in use, unable to allocate a new plugin"
                ));
            }
        }

        Ok(self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }

    pub fn insert(&mut self, plugin: Plugin) -> PluginIndex {
        // Generate a new plugin ID
        let id: i32 = match self.next_id() {
            Ok(id) => id,
            Err(e) => {
                error!("Error creating Plugin: {:?}", e);
                self.set_error(e);
                return -1;
            }
        };
        self.plugins.insert(id, UnsafeCell::new(plugin));
        id
    }

    pub fn new_plugin(&mut self, data: impl AsRef<[u8]>, with_wasi: bool) -> PluginIndex {
        let plugin = match Plugin::new(data, with_wasi) {
            Ok(x) => x,
            Err(e) => {
                error!("Error creating Plugin: {:?}", e);
                self.set_error(e);
                return -1;
            }
        };
        self.insert(plugin)
    }

    pub fn new_plugin_with_functions(
        &mut self,
        data: impl AsRef<[u8]>,
        imports: impl IntoIterator<Item = &'static Function>,
        with_wasi: bool,
    ) -> PluginIndex {
        let plugin = match Plugin::new_with_functions(data, imports, with_wasi) {
            Ok(x) => x,
            Err(e) => {
                error!("Error creating Plugin: {:?}", e);
                self.set_error(e);
                return -1;
            }
        };
        self.insert(plugin)
    }

    /// Set the context error
    pub fn set_error(&mut self, e: impl std::fmt::Debug) {
        trace!("Set context error: {:?}", e);
        self.error = Some(error_string(e));
    }

    /// Convenience function to set error and return the value passed as the final parameter
    pub fn error<T>(&mut self, e: impl std::fmt::Debug, x: T) -> T {
        self.set_error(e);
        x
    }

    /// Get a plugin from the context
    pub fn plugin(&mut self, id: PluginIndex) -> Option<*mut Plugin> {
        match self.plugins.get_mut(&id) {
            Some(x) => Some(x.get_mut()),
            None => None,
        }
    }

    pub fn plugin_exists(&mut self, id: PluginIndex) -> bool {
        self.plugins.contains_key(&id)
    }

    /// Remove a plugin from the context
    pub fn remove(&mut self, id: PluginIndex) {
        if self.plugins.remove(&id).is_some() {
            // Collect old IDs in case we need to re-use them
            self.reclaimed_ids.push_back(id);
        }
    }
}
