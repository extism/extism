use std::collections::BTreeMap;

use crate::*;

#[derive(Default)]
pub struct Context {
    pub plugins: BTreeMap<i32, Plugin>,
    pub error: Option<std::ffi::CString>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            plugins: BTreeMap::new(),
            error: None,
        }
    }

    pub fn set_error(&mut self, e: impl std::fmt::Debug) {
        self.error = Some(error_string(e));
    }

    pub fn plugin(&mut self, id: i32) -> Option<&mut Plugin> {
        self.plugins.get_mut(&id)
    }
}
