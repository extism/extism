#![allow(clippy::missing_safety_doc)]

use std::os::raw::c_char;
use std::str::FromStr;

use crate::*;

#[no_mangle]
pub unsafe extern "C" fn extism_plugin_register(
    wasm: *const u8,
    wasm_size: Size,
    with_wasi: bool,
) -> PluginIndex {
    let data = std::slice::from_raw_parts(wasm, wasm_size as usize);
    let plugin = match Plugin::new(data, with_wasi) {
        Ok(x) => x,
        Err(e) => {
            error!("Error creating Plugin: {:?}", e);
            return -1;
        }
    };

    let mut plugins = match PLUGINS.lock() {
        Ok(p) => p,
        Err(e) => e.into_inner(),
    };

    // Acquire lock and add plugin to registry
    plugins.push(plugin);
    let id = (plugins.len() - 1) as PluginIndex;
    info!("New plugin added: {id}");
    return id;
}

#[no_mangle]
pub unsafe extern "C" fn extism_plugin_config(
    plugin: PluginIndex,
    json: *const u8,
    json_size: Size,
) -> bool {
    let mut plugin = PluginRef::new(plugin);

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

    // Write input to memory
    let data = std::slice::from_raw_parts(data, data_len as usize);
    let handle = match plugin.memory.alloc_bytes(data) {
        Ok(x) => x,
        Err(e) => return plugin.error(e.context("Unable to allocate bytes"), -1),
    };

    #[cfg(feature = "debug")]
    plugin.dump_memory();

    // Always needs to be called before `func.call()`
    plugin.set_input(handle);

    // Call function with offset+length pointing to input data.
    let mut results = vec![Val::I32(0)];
    match func.call(&mut plugin.memory.store, &[], results.as_mut_slice()) {
        Ok(r) => r,
        Err(e) => {
            #[cfg(feature = "debug")]
            plugin.dump_memory();
            error!("Call: {e:?}");
            return plugin.error(e.context("Call failed"), -1);
        }
    };

    #[cfg(feature = "debug")]
    plugin.dump_memory();

    // Return result to caller
    results[0].unwrap_i32()
}

#[no_mangle]
pub unsafe extern "C" fn extism_error(plugin: PluginIndex) -> *const c_char {
    let plugin = PluginRef::new(plugin);
    match &plugin.as_ref().last_error {
        Some(e) => e.as_ptr() as *const _,
        None => std::ptr::null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn extism_output_length(plugin: PluginIndex) -> Size {
    let plugin = PluginRef::new(plugin);

    plugin.as_ref().memory.store.data().output_length as Size
}

#[no_mangle]
pub unsafe extern "C" fn extism_output_get(plugin: PluginIndex, buf: *mut u8, len: Size) {
    let plugin = PluginRef::new(plugin);
    let data = plugin.as_ref().memory.store.data();

    let slice = std::slice::from_raw_parts_mut(buf, len as usize);
    plugin
        .as_ref()
        .memory
        .read(
            MemoryBlock::new(data.output_offset, data.output_length),
            slice,
        )
        .expect("Out of bounds read in extism_output_get");
}

#[no_mangle]
pub unsafe extern "C" fn extism_log_file(
    filename: *const c_char,
    log_level: *const c_char,
) -> bool {
    use log::LevelFilter;
    use log4rs::append::file::FileAppender;
    use log4rs::config::{Appender, Config, Root};
    use log4rs::encode::pattern::PatternEncoder;

    let file = std::ffi::CStr::from_ptr(filename);
    let file = match file.to_str() {
        Ok(x) => x,
        Err(_) => {
            return false;
        }
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

    let logfile = match FileAppender::builder()
        .encoder(Box::new(PatternEncoder::new("{d}: {l} - {m}\n")))
        .build(file)
    {
        Ok(x) => x,
        Err(e) => {
            error!("Unable to set up log encoder: {e:?}");
            return false;
        }
    };

    let config = match Config::builder()
        .appender(Appender::builder().build("logfile", Box::new(logfile)))
        .build(Root::builder().appender("logfile").build(level))
    {
        Ok(x) => x,
        Err(e) => {
            error!("Unable to configure log file: {e:?}");
            return false;
        }
    };

    log4rs::init_config(config).expect("Log initialization failed");
    true
}
