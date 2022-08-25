pub use anyhow::Error;
pub(crate) use wasmtime::*;

pub(crate) mod export;
pub mod manifest;
mod memory;
mod plugin;
mod plugin_ref;
pub mod sdk;

pub use manifest::Manifest;
pub use memory::{MemoryBlock, PluginMemory};
pub use plugin::{Internal, Plugin, PLUGINS};
pub use plugin_ref::PluginRef;

pub type Size = u64;
pub type PluginIndex = i32;
