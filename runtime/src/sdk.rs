#![allow(clippy::missing_safety_doc)]

use std::os::raw::c_char;
use std::str::FromStr;

use crate::*;

/// A union type for host function argument/return values
#[repr(C)]
pub union ValUnion {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
    // TODO: v128, ExternRef, FuncRef
}

/// `ExtismVal` holds the type and value of a function argument/return
#[repr(C)]
pub struct ExtismVal {
    t: ValType,
    v: ValUnion,
}

#[repr(C)]
pub struct ExtismPluginResult {
    pub plugin: *mut Plugin,
    pub error: *mut std::ffi::c_char,
}

/// Host function signature
pub type ExtismFunctionType = extern "C" fn(
    plugin: *mut CurrentPlugin,
    inputs: *const ExtismVal,
    n_inputs: Size,
    outputs: *mut ExtismVal,
    n_outputs: Size,
    data: *mut std::ffi::c_void,
);

impl From<&wasmtime::Val> for ExtismVal {
    fn from(value: &wasmtime::Val) -> Self {
        match value.ty() {
            wasmtime::ValType::I32 => ExtismVal {
                t: ValType::I32,
                v: ValUnion {
                    i32: value.unwrap_i32(),
                },
            },
            wasmtime::ValType::I64 => ExtismVal {
                t: ValType::I64,
                v: ValUnion {
                    i64: value.unwrap_i64(),
                },
            },
            wasmtime::ValType::F32 => ExtismVal {
                t: ValType::F32,
                v: ValUnion {
                    f32: value.unwrap_f32(),
                },
            },
            wasmtime::ValType::F64 => ExtismVal {
                t: ValType::F64,
                v: ValUnion {
                    f64: value.unwrap_f64(),
                },
            },
            t => todo!("{}", t),
        }
    }
}

/// Get a plugin's ID, the returned bytes are a 16 byte buffer that represent a UUID value
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_id(plugin: *mut Plugin) -> *const u8 {
    if plugin.is_null() {
        return std::ptr::null_mut();
    }

    let plugin = &mut *plugin;
    plugin.id.as_bytes().as_ptr()
}

/// Returns a pointer to the memory of the currently running plugin
/// NOTE: this should only be called from host functions.
#[no_mangle]
pub unsafe extern "C" fn extism_current_plugin_memory(plugin: *mut CurrentPlugin) -> *mut u8 {
    if plugin.is_null() {
        return std::ptr::null_mut();
    }

    let plugin = &mut *plugin;
    plugin.memory_ptr()
}

/// Allocate a memory block in the currently running plugin
/// NOTE: this should only be called from host functions.
#[no_mangle]
pub unsafe extern "C" fn extism_current_plugin_memory_alloc(
    plugin: *mut CurrentPlugin,
    n: Size,
) -> u64 {
    if plugin.is_null() {
        return 0;
    }

    let plugin = &mut *plugin;
    plugin.memory_alloc(n as u64).unwrap_or_default()
}

/// Get the length of an allocated block
/// NOTE: this should only be called from host functions.
#[no_mangle]
pub unsafe extern "C" fn extism_current_plugin_memory_length(
    plugin: *mut CurrentPlugin,
    n: Size,
) -> Size {
    if plugin.is_null() {
        return 0;
    }

    let plugin = &mut *plugin;
    plugin.memory_length(n)
}

/// Free an allocated memory block
/// NOTE: this should only be called from host functions.
#[no_mangle]
pub unsafe extern "C" fn extism_current_plugin_memory_free(plugin: *mut CurrentPlugin, ptr: u64) {
    if plugin.is_null() {
        return;
    }

    let plugin = &mut *plugin;
    plugin.memory_free(ptr);
}

/// Create a new host function
///
/// Arguments
/// - `name`: function name, this should be valid UTF-8
/// - `inputs`: argument types
/// - `n_inputs`: number of argument types
/// - `outputs`: return types
/// - `n_outputs`: number of return types
/// - `func`: the function to call
/// - `user_data`: a pointer that will be passed to the function when it's called
///    this value should live as long as the function exists
/// - `free_user_data`: a callback to release the `user_data` value when the resulting
///   `ExtismFunction` is freed.
///
/// Returns a new `ExtismFunction` or `null` if the `name` argument is invalid.
#[no_mangle]
pub unsafe extern "C" fn extism_function_new(
    name: *const std::ffi::c_char,
    inputs: *const ValType,
    n_inputs: Size,
    outputs: *const ValType,
    n_outputs: Size,
    func: ExtismFunctionType,
    user_data: *mut std::ffi::c_void,
    free_user_data: Option<extern "C" fn(_: *mut std::ffi::c_void)>,
) -> *mut Function {
    let name = match std::ffi::CStr::from_ptr(name).to_str() {
        Ok(x) => x.to_string(),
        Err(_) => {
            return std::ptr::null_mut();
        }
    };

    let inputs = if inputs.is_null() || n_inputs == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(inputs, n_inputs as usize)
    }
    .to_vec();

    let output_types = if outputs.is_null() || n_outputs == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(outputs, n_outputs as usize)
    }
    .to_vec();

    let user_data = UserData::new_pointer(user_data, free_user_data);
    let f = Function::new(
        name,
        inputs,
        output_types.clone(),
        Some(user_data),
        move |plugin, inputs, outputs, user_data| {
            let inputs: Vec<_> = inputs.iter().map(ExtismVal::from).collect();
            let mut output_tmp: Vec<_> = output_types
                .iter()
                .map(|t| ExtismVal {
                    t: t.clone(),
                    v: ValUnion { i64: 0 },
                })
                .collect();

            func(
                plugin,
                inputs.as_ptr(),
                inputs.len() as Size,
                output_tmp.as_mut_ptr(),
                output_tmp.len() as Size,
                user_data.as_ptr(),
            );

            for (tmp, out) in output_tmp.iter().zip(outputs.iter_mut()) {
                match tmp.t {
                    ValType::I32 => *out = Val::I32(tmp.v.i32),
                    ValType::I64 => *out = Val::I64(tmp.v.i64),
                    ValType::F32 => *out = Val::F32(tmp.v.f32 as u32),
                    ValType::F64 => *out = Val::F64(tmp.v.f64 as u64),
                    _ => todo!(),
                }
            }
            Ok(())
        },
    );
    Box::into_raw(Box::new(f))
}

/// Free `ExtismFunction`
#[no_mangle]
pub unsafe extern "C" fn extism_function_free(f: *mut Function) {
    drop(Box::from_raw(f))
}

/// Set the namespace of an `ExtismFunction`
#[no_mangle]
pub unsafe extern "C" fn extism_function_set_namespace(
    ptr: *mut Function,
    namespace: *const std::ffi::c_char,
) {
    let namespace = std::ffi::CStr::from_ptr(namespace);
    let f = &mut *ptr;
    f.set_namespace(namespace.to_string_lossy().to_string());
}

/// Create a new plugin with additional host functions
///
/// `wasm`: is a WASM module (wat or wasm) or a JSON encoded manifest
/// `wasm_size`: the length of the `wasm` parameter
/// `functions`: an array of `ExtismFunction*`
/// `n_functions`: the number of functions provided
/// `with_wasi`: enables/disables WASI
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_new(
    wasm: *const u8,
    wasm_size: Size,
    functions: *mut *const Function,
    n_functions: Size,
    with_wasi: bool,
    errmsg: *mut *mut std::ffi::c_char,
) -> *mut Plugin {
    trace!("Call to extism_plugin_new with wasm pointer {:?}", wasm);
    let data = std::slice::from_raw_parts(wasm, wasm_size as usize);
    let mut funcs = vec![];

    if !functions.is_null() {
        for i in 0..n_functions {
            unsafe {
                let f = *functions.add(i as usize);
                if f.is_null() {
                    continue;
                }
                let f = (&*f).clone();
                funcs.push(f);
            }
        }
    }

    let plugin = Plugin::new(data, funcs, with_wasi);
    match plugin {
        Err(e) => {
            if !errmsg.is_null() {
                let e =
                    std::ffi::CString::new(format!("Unable to create plugin: {:?}", e)).unwrap();
                *errmsg = e.into_raw();
            }
            std::ptr::null_mut()
        }
        Ok(p) => Box::into_raw(Box::new(p)),
    }
}

#[no_mangle]
pub unsafe extern "C" fn extism_plugin_error_free(err: *mut std::ffi::c_char) {
    drop(std::ffi::CString::from_raw(err))
}

/// Remove a plugin from the registry and free associated memory
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_free(plugin: *mut Plugin) {
    if plugin.is_null() {
        return;
    }

    let plugin = Box::from_raw(plugin);
    trace!("Freeing plugin {}", plugin.id);
    drop(plugin)
}

#[derive(Clone)]
pub struct ExtismCancelHandle {
    pub(crate) timer_tx: std::sync::mpsc::Sender<TimerAction>,
    pub id: uuid::Uuid,
}

impl ExtismCancelHandle {
    pub fn cancel(&self) -> Result<(), Error> {
        self.timer_tx.send(TimerAction::Cancel { id: self.id })?;
        Ok(())
    }
}

/// Get plugin ID for cancellation
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_cancel_handle(
    plugin: *const Plugin,
) -> *const ExtismCancelHandle {
    if plugin.is_null() {
        return std::ptr::null();
    }
    let plugin = &*plugin;
    &plugin.cancel_handle as *const _
}

/// Cancel a running plugin
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_cancel(handle: *const ExtismCancelHandle) -> bool {
    let handle = &*handle;
    handle.cancel().is_ok()
}

/// Update plugin config values, this will merge with the existing values
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_config(
    plugin: *mut Plugin,
    json: *const u8,
    json_size: Size,
) -> bool {
    if plugin.is_null() {
        return false;
    }
    let plugin = &mut *plugin;
    let _lock = plugin.instance.clone();
    let _lock = _lock.lock().unwrap();
    plugin.current_plugin_mut().clear_error();

    trace!(
        "Call to extism_plugin_config for {} with json pointer {:?}",
        plugin.id,
        json
    );
    let data = std::slice::from_raw_parts(json, json_size as usize);
    let json: std::collections::BTreeMap<String, Option<String>> =
        match serde_json::from_slice(data) {
            Ok(x) => x,
            Err(e) => {
                return plugin.current_plugin_mut().return_error(e, false);
            }
        };

    let wasi = &mut plugin.current_plugin_mut().wasi;
    if let Some(Wasi { ctx, .. }) = wasi {
        for (k, v) in json.iter() {
            match v {
                Some(v) => {
                    let _ = ctx.push_env(k, v);
                }
                None => {
                    let _ = ctx.push_env(k, "");
                }
            }
        }
    }

    let config = &mut plugin.current_plugin_mut().manifest.config;
    for (k, v) in json.into_iter() {
        match v {
            Some(v) => {
                trace!("Config, adding {k}");
                config.insert(k, v);
            }
            None => {
                trace!("Config, removing {k}");
                config.remove(&k);
            }
        }
    }

    true
}

/// Returns true if `func_name` exists
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_function_exists(
    plugin: *mut Plugin,
    func_name: *const c_char,
) -> bool {
    if plugin.is_null() {
        return false;
    }
    let plugin = &mut *plugin;
    let _lock = plugin.instance.clone();
    let _lock = _lock.lock().unwrap();
    plugin.current_plugin_mut().clear_error();

    let name = std::ffi::CStr::from_ptr(func_name);
    trace!("Call to extism_plugin_function_exists for: {:?}", name);

    let name = match name.to_str() {
        Ok(x) => x,
        Err(e) => {
            return plugin.current_plugin_mut().return_error(e, false);
        }
    };

    plugin.function_exists(name)
}

/// Call a function
///
/// `func_name`: is the function to call
/// `data`: is the input data
/// `data_len`: is the length of `data`
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_call(
    plugin: *mut Plugin,
    func_name: *const c_char,
    data: *const u8,
    data_len: Size,
) -> i32 {
    if plugin.is_null() {
        return -1;
    }
    let plugin = &mut *plugin;
    let lock = plugin.instance.clone();
    let mut lock = lock.lock().unwrap();
    plugin.current_plugin_mut().clear_error();

    // Get function name
    let name = std::ffi::CStr::from_ptr(func_name);
    let name = match name.to_str() {
        Ok(name) => name,
        Err(e) => return plugin.current_plugin_mut().return_error(e, -1),
    };

    trace!("Calling function {} of plugin {}", name, plugin.id);
    let input = std::slice::from_raw_parts(data, data_len as usize);
    let res = plugin.raw_call(&mut lock, name, input);

    match res {
        Err((e, rc)) => plugin.current_plugin_mut().return_error(e, rc),
        Ok(x) => x,
    }
}

/// Get the error associated with a `Plugin`
#[no_mangle]
#[deprecated]
pub unsafe extern "C" fn extism_error(plugin: *mut Plugin) -> *const c_char {
    extism_plugin_error(plugin)
}

/// Get the error associated with a `Plugin`
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_error(plugin: *mut Plugin) -> *const c_char {
    if plugin.is_null() {
        return std::ptr::null();
    }
    let plugin = &mut *plugin;
    let _lock = plugin.instance.clone();
    let _lock = _lock.lock().unwrap();
    trace!("Call to extism_error for plugin {}", plugin.id);

    if plugin.output.error_offset == 0 {
        trace!("Error is NULL");
        return std::ptr::null();
    }

    plugin
        .current_plugin_mut()
        .memory_ptr()
        .add(plugin.output.error_offset as usize) as *const _
}

/// Get the length of a plugin's output data
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_output_length(plugin: *mut Plugin) -> Size {
    if plugin.is_null() {
        return 0;
    }
    let plugin = &mut *plugin;
    let _lock = plugin.instance.clone();
    let _lock = _lock.lock().unwrap();
    trace!("Output length: {}", plugin.output.length);
    plugin.output.length
}

/// Get a pointer to the output data
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_output_data(plugin: *mut Plugin) -> *const u8 {
    if plugin.is_null() {
        return std::ptr::null();
    }
    let plugin = &mut *plugin;
    let _lock = plugin.instance.clone();
    let _lock = _lock.lock().unwrap();
    trace!("Call to extism_plugin_output_data for plugin {}", plugin.id);

    let ptr = plugin.current_plugin_mut().memory_ptr();
    ptr.add(plugin.output.offset as usize)
}

/// Set log file and level
#[no_mangle]
pub unsafe extern "C" fn extism_log_file(
    filename: *const c_char,
    log_level: *const c_char,
) -> bool {
    use log::Level;

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

    let level = if !log_level.is_null() {
        let level = std::ffi::CStr::from_ptr(log_level);
        match level.to_str() {
            Ok(x) => x,
            Err(_) => {
                return false;
            }
        }
    } else {
        "error"
    };

    let level = match Level::from_str(&level.to_ascii_lowercase()) {
        Ok(x) => x,
        Err(_) => {
            return false;
        }
    };

    set_log_file(file, level).is_ok()
}

/// Get the Extism version string
#[no_mangle]
pub unsafe extern "C" fn extism_version() -> *const c_char {
    VERSION.as_ptr() as *const _
}
