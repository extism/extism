use extism::{Context, Plugin};
use rustler::{Atom, Env, ResourceArc, Term};
use std::mem;
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

struct ExtismContext {
    ctx: RwLock<Context>,
}
unsafe impl Sync for ExtismContext {}
unsafe impl Send for ExtismContext {}

struct ExtismCancelHandle {
    handle: RwLock<extism::CancelHandle>,
}

unsafe impl Sync for ExtismCancelHandle {}
unsafe impl Send for ExtismCancelHandle {}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(ExtismContext, env);
    rustler::resource!(ExtismCancelHandle, env);
    true
}

fn to_rustler_error(extism_error: extism::Error) -> rustler::Error {
    rustler::Error::Term(Box::new(extism_error.to_string()))
}

#[rustler::nif]
fn context_new() -> ResourceArc<ExtismContext> {
    ResourceArc::new(ExtismContext {
        ctx: RwLock::new(Context::new()),
    })
}

#[rustler::nif]
fn context_reset(ctx: ResourceArc<ExtismContext>) {
    let context = &mut ctx.ctx.write().unwrap();
    context.reset()
}

#[rustler::nif]
fn context_free(ctx: ResourceArc<ExtismContext>) {
    let context = ctx.ctx.read().unwrap();
    std::mem::drop(context)
}

#[rustler::nif]
fn plugin_new_with_manifest(
    ctx: ResourceArc<ExtismContext>,
    manifest_payload: String,
    wasi: bool,
) -> Result<i32, rustler::Error> {
    let context = &ctx.ctx.write().unwrap();
    let result = match Plugin::new(context, manifest_payload, [], wasi) {
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
fn plugin_call(
    ctx: ResourceArc<ExtismContext>,
    plugin_id: i32,
    name: String,
    input: String,
) -> Result<String, rustler::Error> {
    let context = &ctx.ctx.read().unwrap();
    let mut plugin = unsafe { Plugin::from_id(plugin_id, context) };
    let result = match plugin.call(name, input) {
        Err(e) => Err(to_rustler_error(e)),
        Ok(result) => match str::from_utf8(result) {
            Ok(output) => Ok(output.to_string()),
            Err(_e) => Err(rustler::Error::Term(Box::new(
                "Could not read output from plugin",
            ))),
        },
    };
    // this forget should be safe because the context will clean up
    // all it's plugins when it is dropped
    mem::forget(plugin);
    result
}

#[rustler::nif]
fn plugin_update_manifest(
    ctx: ResourceArc<ExtismContext>,
    plugin_id: i32,
    manifest_payload: String,
    wasi: bool,
) -> Result<(), rustler::Error> {
    let context = &ctx.ctx.read().unwrap();
    let mut plugin = unsafe { Plugin::from_id(plugin_id, context) };
    let result = match plugin.update(manifest_payload, [], wasi) {
        Ok(()) => Ok(()),
        Err(e) => Err(to_rustler_error(e)),
    };
    // this forget should be safe because the context will clean up
    // all it's plugins when it is dropped
    mem::forget(plugin);
    result
}

#[rustler::nif]
fn plugin_cancel_handle(
    ctx: ResourceArc<ExtismContext>,
    plugin_id: i32,
) -> Result<ResourceArc<ExtismCancelHandle>, rustler::Error> {
    let context = &ctx.ctx.read().unwrap();
    let plugin = unsafe { Plugin::from_id(plugin_id, context) };
    let handle = plugin.cancel_handle();
    Ok(ResourceArc::new(ExtismCancelHandle {
        handle: RwLock::new(handle),
    }))
}

#[rustler::nif]
fn plugin_cancel(handle: ResourceArc<ExtismCancelHandle>) -> bool {
    handle.handle.read().unwrap().cancel()
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
        Err(_e) => Err(rustler::Error::Term(Box::new(format!(
            "{} not a valid log level",
            log_level
        )))),
        Ok(level) => {
            if extism::set_log_file(path, Some(level)) {
                Ok(atoms::ok())
            } else {
                Err(rustler::Error::Term(Box::new(
                    "Did not set log file, received false from the API.",
                )))
            }
        }
    }
}

#[rustler::nif]
fn plugin_has_function(
    ctx: ResourceArc<ExtismContext>,
    plugin_id: i32,
    function_name: String,
) -> Result<bool, rustler::Error> {
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
        plugin_cancel_handle,
        plugin_cancel,
        plugin_free,
        set_log_file,
    ],
    load = load
);
