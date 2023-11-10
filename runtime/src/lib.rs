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
pub(crate) use plugin_builder::DebugOptions;
pub(crate) use timer::{Timer, TimerAction};
pub(crate) use tracing::{debug, error, trace, warn};

#[cfg(test)]
mod tests;

pub(crate) const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), "\0");

/// Returns a string containing the Extism version of the current runtime, this is the same as the Cargo package
/// version
pub fn extism_version() -> &'static str {
    VERSION
}

/// Set the log file Extism will use, this is a global configuration
fn set_log_file(
    log_file: impl Into<std::path::PathBuf>,
    level: tracing::level_filters::LevelFilter,
) -> Result<(), Error> {
    let log_file = log_file.into();
    let s = log_file.to_str();

    let cfg = tracing_subscriber::FmtSubscriber::builder().with_env_filter(
        tracing_subscriber::EnvFilter::builder().parse(format!("extism={}", level))?,
    );

    if s == Some("-") || s == Some("stderr") {
        cfg.with_ansi(true).with_writer(std::io::stderr).init();
    } else if s == Some("stdout") {
        cfg.with_ansi(true).with_writer(std::io::stdout).init();
    } else {
        let log_file = log_file.to_path_buf();
        let f = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(log_file)
            .expect("Open log file");
        cfg.with_ansi(false)
            .with_writer(move || f.try_clone().unwrap())
            .init();
    };
    Ok(())
}
