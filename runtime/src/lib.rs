pub(crate) use extism_convert::*;
pub(crate) use std::collections::BTreeMap;
pub(crate) use wasmtime::*;

pub use extism_convert as convert;

pub use anyhow::Error;

mod current_plugin;
mod function;
mod internal;
pub(crate) mod manifest;
pub(crate) mod pdk;
mod plugin;
mod plugin_builder;
mod timer;

/// Extism C API
pub mod sdk;

pub use current_plugin::CurrentPlugin;
pub use extism_convert::{FromBytes, FromBytesOwned, ToBytes};
pub use extism_manifest::{Manifest, Wasm, WasmMetadata};
pub use function::{Function, UserData, Val, ValType, PTR};
pub use plugin::{CancelHandle, Plugin, EXTISM_ENV_MODULE, EXTISM_USER_MODULE};
pub use plugin_builder::PluginBuilder;

pub(crate) use internal::{Internal, Wasi};
pub(crate) use log::{debug, error, trace};
pub(crate) use plugin_builder::DebugOptions;
pub(crate) use timer::{Timer, TimerAction};

#[cfg(test)]
mod tests;

pub(crate) const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), "\0");

/// Returns a string containing the Extism version of the current runtime, this is the same as the Cargo package
/// version
pub fn extism_version() -> &'static str {
    VERSION
}

/// Set the log file Extism will use, this is a global configuration
pub fn set_log_file(file: impl AsRef<std::path::Path>, level: log::Level) -> Result<(), Error> {
    let log_file = file.as_ref();
    let s = log_file.to_str();

    let mut d = fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{} {} {}] {}",
                humantime::format_rfc3339_seconds(std::time::SystemTime::now()),
                record.level(),
                record.target(),
                message
            ))
        })
        .level(log::LevelFilter::Off)
        .level_for("extism", level.to_level_filter());

    if s == Some("-") || s == Some("stderr") {
        d = d.chain(std::io::stderr());
    } else if s == Some("stdout") {
        d = d.chain(std::io::stdout());
    } else {
        d = d.chain(fern::log_file(log_file)?);
    }

    d.apply()?;
    Ok(())
}
