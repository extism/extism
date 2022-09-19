use std::collections::BTreeMap;

use crate::*;

#[derive(Default)]
pub struct Context {
    pub plugins: BTreeMap<i32, Plugin>,
    pub error: Option<std::ffi::CString>,
    pub next_id: std::sync::atomic::AtomicI32,
}

impl Context {
    pub fn new() -> Context {
        Context {
            plugins: BTreeMap::new(),
            error: None,
            next_id: std::sync::atomic::AtomicI32::new(0),
        }
    }

    pub fn incr_id(&mut self) -> PluginIndex {
        self.next_id
            .fetch_add(1, std::sync::atomic::Ordering::SeqCst)
    }

    pub fn set_error(&mut self, e: impl std::fmt::Debug) {
        self.error = Some(error_string(e));
    }

    pub fn plugin(&mut self, id: i32) -> Option<&mut Plugin> {
        self.plugins.get_mut(&id)
    }
}
