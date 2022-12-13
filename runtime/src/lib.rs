pub use anyhow::Error;
pub(crate) use wasmtime::*;

mod context;
mod function;
pub mod manifest;
mod memory;
pub(crate) mod pdk;
mod plugin;
mod plugin_ref;
pub mod sdk;

pub use context::Context;
pub(crate) use context::TimerAction;
pub use function::{Function, ValType};
pub use manifest::Manifest;
pub use memory::{MemoryBlock, PluginMemory};
pub use plugin::{Internal, Plugin, Wasi};
pub use plugin_ref::PluginRef;

pub type Size = u64;
pub type PluginIndex = i32;

pub(crate) use log::{debug, error, info, trace};

/// Converts any type implementing `std::fmt::Debug` into a suitable CString to use
/// as an error message
pub(crate) fn error_string(e: impl std::fmt::Debug) -> std::ffi::CString {
    let x = format!("{:?}", e).into_bytes();
    let x = if x[0] == b'"' && x[x.len() - 1] == b'"' {
        x[1..x.len() - 1].to_vec()
    } else {
        x
    };
    unsafe { std::ffi::CString::from_vec_unchecked(x) }
}
