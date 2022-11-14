use rustler::{Atom, Env, Term, ResourceArc};
use extism::{Plugin, Context};
use std::str;
use std::path::Path;
use std::str::FromStr;
use std::sync::RwLock;
use std::mem;

mod atoms {
    rustler::atoms! {
        ok,
        error,
        unknown // Other error
    }
}

struct ExtismContext {
    ctx: RwLock<Context>
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(ExtismContext, env);
    true
}

fn to_rustler_error(extism_error: extism::Error) -> rustler::Error {
    match extism_error {
        extism::Error::UnableToLoadPlugin(msg) => rustler::Error::Term(Box::new(msg)),
        extism::Error::Message(msg) => rustler::Error::Term(Box::new(msg)),
        extism::Error::Json(json_err) => rustler::Error::Term(Box::new(json_err.to_string()))
    }
}

#[rustler::nif]
fn context_new() -> ResourceArc<ExtismContext> {
    ResourceArc::new(
        ExtismContext { ctx: RwLock::new(Context::new()) }
    )
}

#[rustler::nif]
fn context_reset(ctx: ResourceArc<ExtismContext>) {
    let context = &mut ctx.ctx.write().unwrap();
    context.reset()
}

#[rustler::nif]
fn context_free(ctx: ResourceArc<ExtismContext>) {
    let context = &ctx.ctx.read().unwrap();
    std::mem::drop(context)
}

#[rustler::nif]
fn plugin_new_with_manifest(ctx: ResourceArc<ExtismContext>, manifest_payload: String, wasi: bool) -> Result<i32, rustler::Error> {
    let context = &ctx.ctx.write().unwrap();
    let result = match Plugin::new(context, manifest_payload, wasi) {
        Err(e) => Err(to_rustler_error(e)),
        Ok(plugin) => {
            let plugin_id = plugin.as_i32();
            // this forget should be safe because the context will clean up
            // all it's plugins when it is dropped
            mem::forget(plugin);
            Ok(plugin_id)
        }
    };
    result
}

#[rustler::nif]
fn plugin_call(ctx: ResourceArc<ExtismContext>, plugin_id: i32, name: String, input: String) -> Result<String, rustler::Error> {
    let context = &ctx.ctx.read().unwrap();
    let plugin = unsafe { Plugin::from_id(plugin_id, context) };
    let result = match plugin.call(name, input) {
        Err(e) => Err(to_rustler_error(e)),
        Ok(result) => {
            match str::from_utf8(&result) {
                Ok(output) => Ok(output.to_string()),
                Err(_e) => Err(rustler::Error::Term(Box::new("Could not read output from plugin")))
            }
        }
    };
    // this forget should be safe because the context will clean up
    // all it's plugins when it is dropped
    mem::forget(plugin);
    result
}

#[rustler::nif]
fn plugin_update_manifest(ctx: ResourceArc<ExtismContext>, plugin_id: i32, manifest_payload: String, wasi: bool) -> Result<(), rustler::Error> {
    let context = &ctx.ctx.read().unwrap();
    let mut plugin = unsafe { Plugin::from_id(plugin_id, context) };
    let result = match plugin.update(manifest_payload, wasi) {
        Ok(()) => {
            Ok(())
        },
        Err(e) => Err(to_rustler_error(e))
    };
    // this forget should be safe because the context will clean up
    // all it's plugins when it is dropped
    mem::forget(plugin);
    result
}

#[rustler::nif]
fn plugin_free(ctx: ResourceArc<ExtismContext>, plugin_id: i32) -> Result<(), rustler::Error> {
    let context = &ctx.ctx.read().unwrap();
    let plugin = unsafe { Plugin::from_id(plugin_id, context) };
    std::mem::drop(plugin);
    Ok(())
}

#[rustler::nif]
fn set_log_file(filename: String, log_level: String) -> Result<Atom, rustler::Error> {
    let path = Path::new(&filename);
    match log::Level::from_str(&log_level) {
        Err(_e) => Err(rustler::Error::Term(Box::new(format!("{} not a valid log level", log_level)))),
        Ok(level) => {
            if extism::set_log_file(path, Some(level)) {
                Ok(atoms::ok())
            } else {
                Err(rustler::Error::Term(Box::new("Did not set log file, received false from the API.")))
            }
        }
    }
}

#[rustler::nif]
fn plugin_has_function(ctx: ResourceArc<ExtismContext>, plugin_id: i32, function_name: String) -> Result<bool, rustler::Error> {
    let context = &ctx.ctx.read().unwrap();
    let plugin = unsafe { Plugin::from_id(plugin_id, context) };
    let has_function = plugin.has_function(function_name);
    // this forget should be safe because the context will clean up
    // all it's plugins when it is dropped
    mem::forget(plugin);
    Ok(has_function)
}

rustler::init!(
    "Elixir.Extism.Native",
    [
        context_new,
        context_reset,
        context_free,
        plugin_new_with_manifest,
        plugin_call,
        plugin_update_manifest,
        plugin_has_function,
        plugin_free,
        set_log_file,
    ],
    load = load
);
