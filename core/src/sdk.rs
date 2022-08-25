#![allow(clippy::missing_safety_doc)]

use std::os::raw::c_char;

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
            eprintln!("Error creating Plugin: {:?}", e);
            return -1;
        }
    };

    // Acquire lock and add plugin to registry
    if let Ok(mut plugins) = PLUGINS.lock() {
        plugins.push(plugin);
        return (plugins.len() - 1) as PluginIndex;
    }

    -1
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
    let name = name.to_str().expect("Invalid function name");
    plugin.as_mut().get_func(name).is_some()
}

#[no_mangle]
pub unsafe extern "C" fn extism_call(
    plugin: PluginIndex,
    func_name: *const c_char,
    data: *const u8,
    data_len: Size,
) -> i32 {
    let mut plugin = PluginRef::new(plugin).init();
    let plugin = plugin.as_mut();

    // Find function
    let name = std::ffi::CStr::from_ptr(func_name);
    let name = name.to_str().expect("Invalid function name");
    let func = plugin
        .get_func(name)
        .expect(&format!("Function not found {name}"));

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
    // TODO: In the future this could be a JSON or Protobuf payload.
    let mut results = vec![Val::I32(0)];
    match func.call(&mut plugin.memory.store, &[], results.as_mut_slice()) {
        Ok(r) => r,
        Err(e) => {
            #[cfg(feature = "debug")]
            plugin.dump_memory();
            return plugin.error(e.context("Invalid write"), -1);
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
