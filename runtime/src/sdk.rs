#![allow(clippy::missing_safety_doc)]

use std::os::raw::c_char;
use std::str::FromStr;

use crate::*;

static mut REGISTER_ERROR: std::sync::Mutex<Option<std::ffi::CString>> =
    std::sync::Mutex::new(None);

#[no_mangle]
pub unsafe extern "C" fn extism_plugin_register(
    wasm: *const u8,
    wasm_size: Size,
    with_wasi: bool,
) -> PluginIndex {
    trace!(
        "Call to extism_plugin_register with wasm pointer {:?}",
        wasm
    );
    let data = std::slice::from_raw_parts(wasm, wasm_size as usize);
    let plugin = match Plugin::new(data, with_wasi) {
        Ok(x) => x,
        Err(e) => {
            error!("Error creating Plugin: {:?}", e);
            let mut error = match REGISTER_ERROR.lock() {
                Ok(x) => x,
                Err(x) => x.into_inner(),
            };

            *error = Some(error_string(e));
            return -1;
        }
    };

    let mut plugins = match PLUGINS.lock() {
        Ok(p) => p,
        Err(e) => {
            debug!("Mutex poisoned");
            e.into_inner()
        }
    };

    for (id, p) in plugins.iter_mut().enumerate() {
        if p.is_none() {
            *p = Some(plugin);
            info!("Plugin added: {id}");
            return id as i32;
        }
    }

    plugins.push(Some(plugin));
    let id = (plugins.len() - 1) as PluginIndex;
    info!("New plugin added: {id}");
    id
}

#[no_mangle]
pub unsafe extern "C" fn extism_plugin_update(
    index: PluginIndex,
    wasm: *const u8,
    wasm_size: Size,
    with_wasi: bool,
) -> bool {
    let index = index as usize;
    trace!("Call to extism_plugin_update with wasm pointer {:?}", wasm);
    let data = std::slice::from_raw_parts(wasm, wasm_size as usize);
    let plugin = match Plugin::new(data, with_wasi) {
        Ok(x) => x,
        Err(e) => {
            error!("Error creating Plugin: {:?}", e);
            let mut error = match REGISTER_ERROR.lock() {
                Ok(x) => x,
                Err(x) => x.into_inner(),
            };
            *error = Some(error_string(e));
            return false;
        }
    };

    let mut plugins = match PLUGINS.lock() {
        Ok(p) => p,
        Err(e) => e.into_inner(),
    };

    if index < plugins.len() {
        plugins[index] = Some(plugin);
    }

    info!("Plugin updated: {index}");
    true
}

#[no_mangle]
pub unsafe extern "C" fn extism_plugin_destroy(plugin: PluginIndex) {
    if plugin < 0 {
        return;
    }

    let mut plugins = match PLUGINS.lock() {
        Ok(p) => p,
        Err(e) => {
            debug!("Mutex poisoned");
            e.into_inner()
        }
    };

    if plugins.len() <= plugin as usize {
        return;
    }

    let p = &mut plugins[plugin as usize];
    if p.is_some() {
        *p = None
    }

    // Attempt to cleanup if there are `None`s at the end of the vec
    for (i, x) in plugins.iter().rev().enumerate() {
        let i = plugins.len() - i;

        // Stop at first `Some`
        if x.is_some() {
            if i < plugins.len() {
                plugins.truncate(i)
            }

            break;
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn extism_reset() {
    let mut plugins = match PLUGINS.lock() {
        Ok(p) => p,
        Err(e) => e.into_inner(),
    };

    plugins.clear();
}

#[no_mangle]
pub unsafe extern "C" fn extism_plugin_config(
    plugin: PluginIndex,
    json: *const u8,
    json_size: Size,
) -> bool {
    let mut plugin = PluginRef::new(plugin);

    trace!(
        "Call to extism_plugin_config for {} with json pointer {:?}",
        plugin.id,
        json
    );

    let data = std::slice::from_raw_parts(json, json_size as usize);
    let json: std::collections::BTreeMap<String, String> = match serde_json::from_slice(data) {
        Ok(x) => x,
        Err(e) => {
            plugin.as_mut().set_error(e);
            return false;
        }
    };

    let plugin = plugin.as_mut();
    let wasi = &mut plugin.memory.store.data_mut().wasi;
    let config = &mut plugin.manifest.as_mut().config;
    for (k, v) in json.into_iter() {
        trace!("Config, adding {k}");
        let _ = wasi.push_env(&k, &v);
        config.insert(k, v);
    }

    true
}

#[no_mangle]
pub unsafe extern "C" fn extism_function_exists(
    plugin: PluginIndex,
    func_name: *const c_char,
) -> bool {
    let mut plugin = PluginRef::new(plugin);

    let name = std::ffi::CStr::from_ptr(func_name);
    let name = match name.to_str() {
        Ok(x) => x,
        Err(_) => return false,
    };

    plugin.as_mut().get_func(name).is_some()
}

#[no_mangle]
pub unsafe extern "C" fn extism_call(
    plugin_id: PluginIndex,
    func_name: *const c_char,
    data: *const u8,
    data_len: Size,
) -> i32 {
    let mut plugin = PluginRef::new(plugin_id).init();
    let plugin = plugin.as_mut();

    // Find function
    let name = std::ffi::CStr::from_ptr(func_name);
    let name = match name.to_str() {
        Ok(name) => name,
        Err(e) => return plugin.error(e, -1),
    };

    debug!("Calling function: {name} in plugin {plugin_id}");

    let func = match plugin.get_func(name) {
        Some(x) => x,
        None => return plugin.error(format!("Function not found: {name}"), -1),
    };

    // Always needs to be called before `func.call()`
    plugin.set_input(data, data_len as usize);

    // Call function with offset+length pointing to input data.
    let mut results = vec![Val::I32(0)];
    match func.call(&mut plugin.memory.store, &[], results.as_mut_slice()) {
        Ok(r) => r,
        Err(e) => {
            plugin.dump_memory();
            error!("Call: {e:?}");
            return plugin.error(e.context("Call failed"), -1);
        }
    };

    plugin.dump_memory();

    // Return result to caller
    results[0].unwrap_i32()
}

#[no_mangle]
pub unsafe extern "C" fn extism_error(plugin: PluginIndex) -> *const c_char {
    trace!("Call to extism_error for plugin {plugin}");

    if plugin < 0 {
        let error = match REGISTER_ERROR.lock() {
            Ok(x) => x,
            Err(x) => x.into_inner(),
        };

        match error.as_ref() {
            Some(e) => return e.as_ptr() as *const _,
            None => return std::ptr::null(),
        }
    }

    let plugin = PluginRef::new(plugin);
    match &plugin.as_ref().last_error {
        Some(e) => e.as_ptr() as *const _,
        None => {
            trace!("Error is NULL");
            std::ptr::null()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn extism_output_length(plugin: PluginIndex) -> Size {
    trace!("Call to extism_output_length for plugin {plugin}");
    let plugin = PluginRef::new(plugin);

    let len = plugin.as_ref().memory.store.data().output_length as Size;
    trace!("Output length: {len}");
    len
}

#[no_mangle]
pub unsafe extern "C" fn extism_output_get(plugin: PluginIndex) -> *const u8 {
    trace!("Call to extism_output_get for plugin {plugin}");

    let plugin = PluginRef::new(plugin);
    let data = plugin.as_ref().memory.store.data();

    plugin
        .as_ref()
        .memory
        .get(MemoryBlock::new(data.output_offset, data.output_length))
        .as_ptr()
}

#[no_mangle]
pub unsafe extern "C" fn extism_log_file(
    filename: *const c_char,
    log_level: *const c_char,
) -> bool {
    use log::LevelFilter;
    use log4rs::append::console::ConsoleAppender;
    use log4rs::append::file::FileAppender;
    use log4rs::config::{Appender, Config, Logger, Root};
    use log4rs::encode::pattern::PatternEncoder;

    let file = if !filename.is_null() {
        let file = std::ffi::CStr::from_ptr(filename);
        match file.to_str() {
            Ok(x) => x,
            Err(_) => {
                return false;
            }
        }
    } else {
        "stderr"
    };

    let level = if log_level.is_null() {
        "error"
    } else {
        let level = std::ffi::CStr::from_ptr(log_level);
        match level.to_str() {
            Ok(x) => x,
            Err(_) => {
                return false;
            }
        }
    };

    let level = match LevelFilter::from_str(level) {
        Ok(x) => x,
        Err(_) => {
            return false;
        }
    };

    let encoder = Box::new(PatternEncoder::new("{t} {l} {d} - {m}\n"));

    let logfile: Box<dyn log4rs::append::Append> =
        if file == "-" || file == "stdout" || file == "stderr" {
            let target = if file == "-" || file == "stdout" {
                log4rs::append::console::Target::Stdout
            } else {
                log4rs::append::console::Target::Stderr
            };
            let console = ConsoleAppender::builder().target(target).encoder(encoder);
            Box::new(console.build())
        } else {
            match FileAppender::builder().encoder(encoder).build(file) {
                Ok(x) => Box::new(x),
                Err(e) => {
                    error!("Unable to set up log encoder: {e:?}");
                    return false;
                }
            }
        };

    let config = match Config::builder()
        .appender(Appender::builder().build("logfile", logfile))
        .logger(Logger::builder().appender("logfile").build("extism", level))
        .build(Root::builder().build(LevelFilter::Off))
    {
        Ok(x) => x,
        Err(_) => {
            return false;
        }
    };

    if log4rs::init_config(config).is_err() {
        return false;
    }
    true
}
