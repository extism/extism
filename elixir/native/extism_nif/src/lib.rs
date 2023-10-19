use extism::Plugin;
use rustler::{Atom, Env, ResourceArc, Term};
use std::path::Path;
use std::str;
use std::str::FromStr;
use std::sync::RwLock;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        unknown // Other error
    }
}

struct ExtismPlugin {
    plugin: RwLock<Option<Plugin>>,
}
unsafe impl Sync for ExtismPlugin {}
unsafe impl Send for ExtismPlugin {}

struct ExtismCancelHandle {
    handle: RwLock<extism::CancelHandle>,
}

unsafe impl Sync for ExtismCancelHandle {}
unsafe impl Send for ExtismCancelHandle {}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(ExtismPlugin, env);
    rustler::resource!(ExtismCancelHandle, env);
    true
}

fn to_rustler_error(extism_error: extism::Error) -> rustler::Error {
    rustler::Error::Term(Box::new(extism_error.to_string()))
}

fn freed_error() -> rustler::Error {
    rustler::Error::Term(Box::new("Plugin has already been freed".to_string()))
}

#[rustler::nif]
fn plugin_new_with_manifest(
    manifest_payload: String,
    wasi: bool,
) -> Result<ResourceArc<ExtismPlugin>, rustler::Error> {
    let result = match Plugin::new(manifest_payload, [], wasi) {
        Err(e) => Err(to_rustler_error(e)),
        Ok(plugin) => Ok(ResourceArc::new(ExtismPlugin {
            plugin: RwLock::new(Some(plugin)),
        })),
    };
    result
}

#[rustler::nif]
fn plugin_call(
    plugin: ResourceArc<ExtismPlugin>,
    name: String,
    input: String,
) -> Result<String, rustler::Error> {
    let mut plugin = plugin.plugin.write().unwrap();
    if let Some(plugin) = &mut *plugin {
        let result = match plugin.call(name, input) {
            Err(e) => Err(to_rustler_error(e)),
            Ok(result) => match str::from_utf8(result) {
                Ok(output) => Ok(output.to_string()),
                Err(_e) => Err(rustler::Error::Term(Box::new(
                    "Could not read output from plugin",
                ))),
            },
        };
        result
    } else {
        Err(freed_error())
    }
}

#[rustler::nif]
fn plugin_cancel_handle(
    plugin: ResourceArc<ExtismPlugin>,
) -> Result<ResourceArc<ExtismCancelHandle>, rustler::Error> {
    let mut plugin = plugin.plugin.write().unwrap();
    if let Some(plugin) = &mut *plugin {
        let handle = plugin.cancel_handle();
        Ok(ResourceArc::new(ExtismCancelHandle {
            handle: RwLock::new(handle),
        }))
    } else {
        Err(freed_error())
    }
}

#[rustler::nif]
fn plugin_cancel(handle: ResourceArc<ExtismCancelHandle>) -> bool {
    handle.handle.read().unwrap().cancel().is_ok()
}

#[rustler::nif]
fn plugin_free(plugin: ResourceArc<ExtismPlugin>) -> Result<(), rustler::Error> {
    let mut plugin = plugin.plugin.write().unwrap();
    if let Some(plugin) = plugin.take() {
        drop(plugin);
    }
    Ok(())
}

#[rustler::nif]
fn set_log_file(filename: String, log_level: String) -> Result<Atom, rustler::Error> {
    let path = Path::new(&filename);
    match log::Level::from_str(&log_level) {
        Err(_e) => Err(rustler::Error::Term(Box::new(format!(
            "{} not a valid log level",
            log_level
        )))),
        Ok(level) => match extism::set_log_file(path, level) {
            Ok(()) => Ok(atoms::ok()),
            Err(e) => Err(rustler::Error::Term(Box::new(format!(
                "Did not set log file: {e:?}"
            )))),
        },
    }
}

#[rustler::nif]
fn plugin_has_function(
    plugin: ResourceArc<ExtismPlugin>,
    function_name: String,
) -> Result<bool, rustler::Error> {
    let mut plugin = plugin.plugin.write().unwrap();
    if let Some(plugin) = &mut *plugin {
        let has_function = plugin.function_exists(function_name);
        Ok(has_function)
    } else {
        Err(freed_error())
    }
}

rustler::init!(
    "Elixir.Extism.Native",
    [
        plugin_new_with_manifest,
        plugin_call,
        plugin_has_function,
        plugin_cancel_handle,
        plugin_cancel,
        plugin_free,
        set_log_file,
    ],
    load = load
);
