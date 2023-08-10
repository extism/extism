pub use anyhow::Error;
pub(crate) use wasmtime::*;

mod function;
mod internal;
pub(crate) mod manifest;
pub(crate) mod pdk;
mod plugin;
pub mod sdk;

pub use extism_manifest::Manifest;
pub use function::{Function, UserData, Val, ValType};
pub use internal::{Internal, InternalExt, Wasi};
pub use plugin::Plugin;

pub type Size = u64;

pub(crate) use log::{debug, error, trace};
