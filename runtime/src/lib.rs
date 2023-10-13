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
pub use extism_manifest::Manifest;
pub use function::{Function, UserData, Val, ValType};
pub use plugin::{CancelHandle, Plugin, EXTISM_ENV};
pub use plugin_builder::PluginBuilder;

pub(crate) use internal::{Internal, Wasi};
pub(crate) use log::{debug, error, trace};
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
    use log4rs::append::console::ConsoleAppender;
    use log4rs::append::file::FileAppender;
    use log4rs::config::{Appender, Config, Logger, Root};
    use log4rs::encode::pattern::PatternEncoder;
    let encoder = Box::new(PatternEncoder::new("{t} {l} {d} - {m}\n"));
    let file = file.as_ref();

    let logfile: Box<dyn log4rs::append::Append> = if file == std::path::PathBuf::from("stdout") {
        let target = log4rs::append::console::Target::Stdout;
        let console = ConsoleAppender::builder().target(target).encoder(encoder);
        Box::new(console.build())
    } else if file == std::path::PathBuf::from("-") || file == std::path::PathBuf::from("stderr") {
        let target = log4rs::append::console::Target::Stderr;
        let console = ConsoleAppender::builder().target(target).encoder(encoder);
        Box::new(console.build())
    } else {
        Box::new(FileAppender::builder().encoder(encoder).build(file)?)
    };

    let config = Config::builder()
        .appender(Appender::builder().build("logfile", logfile))
        .logger(
            Logger::builder()
                .appender("logfile")
                .build("extism", level.to_level_filter()),
        )
        .build(Root::builder().build(log::LevelFilter::Off))?;

    log4rs::init_config(config)?;
    Ok(())
}
