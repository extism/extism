// Makes proc-macros able to resolve `::extism` correctly
extern crate self as extism;

macro_rules! catch_out_of_fuel {
    ($store: expr, $x:expr) => {{
        let y = $x;
        if y.is_err() && $store.get_fuel().is_ok_and(|x| x == 0) {
            Err(Error::msg("plugin ran out of fuel"))
        } else {
            y
        }
    }};
}

pub(crate) use extism_convert::*;
pub(crate) use std::collections::BTreeMap;
use std::str::FromStr;
pub(crate) use wasmtime::*;

#[doc(inline)]
pub use extism_convert as convert;

pub use anyhow::Error;

mod current_plugin;
mod function;
mod internal;
pub(crate) mod manifest;
pub(crate) mod pdk;
mod plugin;
mod plugin_builder;
mod pool;
mod readonly_dir;
mod timer;

/// Extism C API
pub mod sdk;

pub use current_plugin::CurrentPlugin;
pub use extism_convert::{FromBytes, FromBytesOwned, ToBytes};
pub use extism_manifest::{Manifest, Wasm, WasmMetadata};
pub use function::{Function, UserData, Val, ValType, PTR};
pub use plugin::{CancelHandle, Plugin, WasmInput, EXTISM_ENV_MODULE, EXTISM_USER_MODULE};
pub use plugin_builder::{DebugOptions, PluginBuilder};
pub use pool::Pool;

pub(crate) use internal::{Internal, Wasi};
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

#[derive(Clone)]
struct LogFunction<F: Clone + Fn(&str)> {
    func: F,
}

unsafe impl<F: Clone + Fn(&str)> Send for LogFunction<F> {}
unsafe impl<F: Clone + Fn(&str)> Sync for LogFunction<F> {}

impl<F: Clone + Fn(&str)> std::io::Write for LogFunction<F> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Ok(s) = std::str::from_utf8(buf) {
            (self.func)(s)
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// Sets a custom callback to handle logs, each line will be passed to the provided callback instead of being
/// logged to a file. This initializes a default `tracing_subscriber` and should only be called once.
///
/// `filter` may contain a general level like `trace` or `error`, but can also be more specific to enable logging only
/// from specific crates. For example, to enable trace-level logging only for the extism crate use: `extism=trace`.
pub fn set_log_callback<F: 'static + Clone + Fn(&str)>(
    func: F,
    filter: impl AsRef<str>,
) -> Result<(), Error> {
    let filter = filter.as_ref();
    let is_level = tracing::Level::from_str(filter).is_ok();
    let cfg = tracing_subscriber::FmtSubscriber::builder().with_env_filter({
        let x = tracing_subscriber::EnvFilter::builder()
            .with_default_directive(tracing::Level::ERROR.into());
        if is_level {
            x.parse_lossy(format!("extism={}", filter))
        } else {
            x.parse_lossy(filter)
        }
    });
    let w = LogFunction { func };
    cfg.with_ansi(false)
        .with_writer(move || w.clone())
        .try_init()
        .map_err(|x| Error::msg(x.to_string()))?;
    Ok(())
}
