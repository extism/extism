#![allow(clippy::missing_safety_doc)]

use std::{os::raw::c_char, ptr::null_mut};

use crate::*;

pub type ExtismMemoryHandle = u64;
pub type Size = u64;
pub struct ExtismFunction(std::cell::Cell<Option<Function>>);

/// The return code used to specify a successful plugin call
pub static EXTISM_SUCCESS: i32 = 0;

fn make_error_msg(s: String) -> Vec<u8> {
    let mut s = s.into_bytes();
    s.push(0);
    s
}

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

/// Host function signature
pub type ExtismFunctionType = extern "C" fn(
    plugin: *mut CurrentPlugin,
    inputs: *const ExtismVal,
    n_inputs: Size,
    outputs: *mut ExtismVal,
    n_outputs: Size,
    data: *mut std::ffi::c_void,
);

/// Log drain callback
pub type ExtismLogDrainFunctionType = extern "C" fn(data: *const std::ffi::c_char, size: Size);

impl ExtismVal {
    fn from_val(value: &wasmtime::Val, ctx: impl AsContext) -> Result<Self, Error> {
        match value.ty(ctx)? {
            wasmtime::ValType::I32 => Ok(ExtismVal {
                t: ValType::I32,
                v: ValUnion {
                    i32: value.unwrap_i32(),
                },
            }),
            wasmtime::ValType::I64 => Ok(ExtismVal {
                t: ValType::I64,
                v: ValUnion {
                    i64: value.unwrap_i64(),
                },
            }),
            wasmtime::ValType::F32 => Ok(ExtismVal {
                t: ValType::F32,
                v: ValUnion {
                    f32: value.unwrap_f32(),
                },
            }),
            wasmtime::ValType::F64 => Ok(ExtismVal {
                t: ValType::F64,
                v: ValUnion {
                    f64: value.unwrap_f64(),
                },
            }),
            t => todo!("{}", t),
        }
    }
}

/// Get a plugin's ID, the returned bytes are a 16 byte buffer that represent a UUIDv4
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_id(plugin: *mut Plugin) -> *const u8 {
    if plugin.is_null() {
        return std::ptr::null_mut();
    }

    let plugin = &mut *plugin;
    plugin.id.as_bytes().as_ptr()
}

/// Get the current plugin's associated host context data. Returns null if call was made without
/// host context.
#[no_mangle]
pub unsafe extern "C" fn extism_current_plugin_host_context(
    plugin: *mut CurrentPlugin,
) -> *mut std::ffi::c_void {
    if plugin.is_null() {
        return std::ptr::null_mut();
    }

    let plugin = &mut *plugin;
    if let Ok(CVoidContainer(ptr)) = plugin.host_context::<CVoidContainer>() {
        *ptr
    } else {
        std::ptr::null_mut()
    }
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
) -> ExtismMemoryHandle {
    if plugin.is_null() {
        return 0;
    }

    let plugin = &mut *plugin;
    match plugin.memory_alloc(n) {
        Ok(x) => x.offset(),
        Err(_) => 0,
    }
}

/// Get the length of an allocated block
/// NOTE: this should only be called from host functions.
#[no_mangle]
pub unsafe extern "C" fn extism_current_plugin_memory_length(
    plugin: *mut CurrentPlugin,
    n: ExtismMemoryHandle,
) -> Size {
    if plugin.is_null() {
        return 0;
    }

    let plugin = &mut *plugin;
    plugin.memory_length(n).unwrap_or_default()
}

/// Free an allocated memory block
/// NOTE: this should only be called from host functions.
#[no_mangle]
pub unsafe extern "C" fn extism_current_plugin_memory_free(
    plugin: *mut CurrentPlugin,
    ptr: ExtismMemoryHandle,
) {
    if plugin.is_null() {
        return;
    }

    let plugin = &mut *plugin;
    if let Some(handle) = plugin.memory_handle(ptr) {
        let _ = plugin.memory_free(handle);
    }
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
///   this value should live as long as the function exists
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
) -> *mut ExtismFunction {
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

    let user_data: UserData<()> = UserData::new_pointer(user_data, free_user_data);
    let f = Function::new(
        name,
        inputs,
        output_types.clone(),
        user_data,
        move |plugin, inputs, outputs, user_data| {
            let store = &*plugin.store;
            let inputs: Vec<_> = inputs
                .iter()
                .map(|x| ExtismVal::from_val(x, store).unwrap())
                .collect();
            let mut output_tmp: Vec<_> = output_types
                .iter()
                .map(|t| ExtismVal {
                    t: t.clone(),
                    v: ValUnion { i64: 0 },
                })
                .collect();

            // We cannot simply "get" the Vec's storage pointer because
            // the underlying storage might be invalid when the Vec is empty.
            // In that case, we return (null, 0).

            let (inputs_ptr, inputs_len) = if inputs.is_empty() {
                (core::ptr::null(), 0 as Size)
            } else {
                (inputs.as_ptr(), inputs.len() as Size)
            };

            let (output_ptr, output_len) = if output_tmp.is_empty() {
                (null_mut(), 0 as Size)
            } else {
                (output_tmp.as_mut_ptr(), output_tmp.len() as Size)
            };

            func(
                plugin,
                inputs_ptr,
                inputs_len,
                output_ptr,
                output_len,
                user_data.as_ptr(),
            );

            for (tmp, out) in output_tmp.iter().zip(outputs.iter_mut()) {
                match tmp.t {
                    ValType::I32 => *out = Val::I32(tmp.v.i32),
                    ValType::I64 => *out = Val::I64(tmp.v.i64),
                    ValType::F32 => *out = Val::F32(tmp.v.f32.to_bits()),
                    ValType::F64 => *out = Val::F64(tmp.v.f64.to_bits()),
                    _ => todo!(),
                }
            }
            Ok(())
        },
    );
    Box::into_raw(Box::new(ExtismFunction(std::cell::Cell::new(Some(f)))))
}

/// Free `ExtismFunction`
#[no_mangle]
pub unsafe extern "C" fn extism_function_free(f: *mut ExtismFunction) {
    if f.is_null() {
        return;
    }

    drop(Box::from_raw(f))
}

/// Set the namespace of an `ExtismFunction`
#[no_mangle]
pub unsafe extern "C" fn extism_function_set_namespace(
    ptr: *mut ExtismFunction,
    namespace: *const std::ffi::c_char,
) {
    let namespace = std::ffi::CStr::from_ptr(namespace);
    let f = &mut *ptr;
    if let Some(x) = f.0.get_mut() {
        x.set_namespace(namespace.to_string_lossy().to_string());
    } else {
        debug!("Trying to set namespace of already registered function")
    }
}

/// Pre-compile an Extism plugin
#[no_mangle]
pub unsafe extern "C" fn extism_compiled_plugin_new(
    wasm: *const u8,
    wasm_size: Size,
    functions: *mut *const ExtismFunction,
    n_functions: Size,
    with_wasi: bool,
    errmsg: *mut *mut std::ffi::c_char,
) -> *mut CompiledPlugin {
    trace!("Call to extism_plugin_new with wasm pointer {:?}", wasm);
    let data = std::slice::from_raw_parts(wasm, wasm_size as usize);

    let mut builder = PluginBuilder::new(data).with_wasi(with_wasi);

    if !functions.is_null() {
        let funcs = (0..n_functions)
            .map(|i| unsafe { *functions.add(i as usize) })
            .map(|ptr| {
                if ptr.is_null() {
                    return Err("Cannot pass null pointer");
                }

                let ExtismFunction(func) = &*ptr;
                let Some(func) = func.take() else {
                    return Err("Function cannot be registered with multiple different Plugins");
                };

                Ok(func)
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap_or_else(|e| {
                if !errmsg.is_null() {
                    let e = std::ffi::CString::new(e.to_string()).unwrap();
                    *errmsg = e.into_raw();
                }
                Vec::new()
            });

        if funcs.len() != n_functions as usize {
            return std::ptr::null_mut();
        }

        builder = builder.with_functions(funcs);
    }

    CompiledPlugin::new(builder)
        .map(|v| Box::into_raw(Box::new(v)))
        .unwrap_or_else(|e| {
            if !errmsg.is_null() {
                let e = std::ffi::CString::new(format!(
                    "Unable to compile Extism plugin: {}",
                    e.root_cause(),
                ))
                .unwrap();
                *errmsg = e.into_raw();
            }
            std::ptr::null_mut()
        })
}

/// Free `ExtismCompiledPlugin`
#[no_mangle]
pub unsafe extern "C" fn extism_compiled_plugin_free(plugin: *mut CompiledPlugin) {
    if plugin.is_null() {
        return;
    }

    let plugin = Box::from_raw(plugin);
    trace!("called extism_compiled_plugin_free");
    drop(plugin)
}

/// Create a new plugin with host functions, the functions passed to this function no longer need to be manually freed using
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
    functions: *mut *const ExtismFunction,
    n_functions: Size,
    with_wasi: bool,
    errmsg: *mut *mut std::ffi::c_char,
) -> *mut Plugin {
    let data = std::slice::from_raw_parts(wasm, wasm_size as usize);
    let funcs = if functions.is_null() {
        vec![]
    } else {
        let funcs = (0..n_functions)
            .map(|i| unsafe { *functions.add(i as usize) })
            .map(|ptr| {
                if ptr.is_null() {
                    return Err("Cannot pass null pointer");
                }

                let ExtismFunction(func) = &*ptr;
                let Some(func) = func.take() else {
                    return Err("Function cannot be registered with multiple different Plugins");
                };

                Ok(func)
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap_or_else(|e| {
                if !errmsg.is_null() {
                    let e = std::ffi::CString::new(e.to_string()).unwrap();
                    *errmsg = e.into_raw();
                }
                Vec::new()
            });

        if funcs.len() != n_functions as usize {
            return std::ptr::null_mut();
        }

        funcs
    };

    Plugin::new(data, funcs, with_wasi)
        .map(|v| Box::into_raw(Box::new(v)))
        .unwrap_or_else(|e| {
            if !errmsg.is_null() {
                let e = std::ffi::CString::new(format!(
                    "Unable to compile Extism plugin: {}",
                    e.root_cause(),
                ))
                .unwrap();
                *errmsg = e.into_raw();
            }
            std::ptr::null_mut()
        })
}

/// Create a new plugin from an `ExtismCompiledPlugin`
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_new_from_compiled(
    compiled: *const CompiledPlugin,
    errmsg: *mut *mut std::ffi::c_char,
) -> *mut Plugin {
    let plugin = Plugin::new_from_compiled(&*compiled);
    match plugin {
        Err(e) => {
            if !errmsg.is_null() {
                let e = std::ffi::CString::new(format!(
                    "Unable to create Extism plugin: {}",
                    e.root_cause(),
                ))
                .unwrap();
                *errmsg = e.into_raw();
            }
            std::ptr::null_mut()
        }
        Ok(p) => Box::into_raw(Box::new(p)),
    }
}

/// Create a new plugin and set the number of instructions a plugin is allowed to execute
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_new_with_fuel_limit(
    wasm: *const u8,
    wasm_size: Size,
    functions: *mut *const ExtismFunction,
    n_functions: Size,
    with_wasi: bool,
    fuel_limit: u64,
    errmsg: *mut *mut std::ffi::c_char,
) -> *mut Plugin {
    trace!(
        "Call to extism_plugin_new_with_fuel_limit with wasm pointer {:?}",
        wasm
    );
    let data = std::slice::from_raw_parts(wasm, wasm_size as usize);
    let funcs = if functions.is_null() {
        vec![]
    } else {
        let funcs = (0..n_functions)
            .map(|i| unsafe { *functions.add(i as usize) })
            .map(|ptr| {
                if ptr.is_null() {
                    return Err("Cannot pass null pointer");
                }

                let ExtismFunction(func) = &*ptr;
                let Some(func) = func.take() else {
                    return Err("Function cannot be registered with multiple different Plugins");
                };

                Ok(func)
            })
            .collect::<Result<Vec<_>, _>>()
            .unwrap_or_else(|e| {
                if !errmsg.is_null() {
                    let e = std::ffi::CString::new(e.to_string()).unwrap();
                    *errmsg = e.into_raw();
                }
                Vec::new()
            });

        if funcs.len() != n_functions as usize {
            return std::ptr::null_mut();
        }

        funcs
    };

    let compiled = match CompiledPlugin::new(
        PluginBuilder::new(data)
            .with_functions(funcs)
            .with_wasi(with_wasi)
            .with_fuel_limit(fuel_limit),
    ) {
        Ok(x) => x,
        Err(e) => {
            if !errmsg.is_null() {
                let e = std::ffi::CString::new(format!(
                    "Unable to compile Extism plugin: {}",
                    e.root_cause(),
                ))
                .unwrap();
                *errmsg = e.into_raw();
            }
            return std::ptr::null_mut();
        }
    };

    let plugin = Plugin::new_from_compiled(&compiled);

    match plugin {
        Err(e) => {
            if !errmsg.is_null() {
                let e = std::ffi::CString::new(format!(
                    "Unable to create Extism plugin: {}",
                    e.root_cause(),
                ))
                .unwrap();
                *errmsg = e.into_raw();
            }
            std::ptr::null_mut()
        }
        Ok(p) => Box::into_raw(Box::new(p)),
    }
}

/// Enable HTTP response headers in plugins using `extism:host/env::http_request`
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_allow_http_response_headers(plugin: *mut Plugin) {
    let plugin = &mut *plugin;
    plugin.store.data_mut().http_headers = Some(BTreeMap::new());
}

/// Free the error returned by `extism_plugin_new`, errors returned from `extism_plugin_error` don't need to be freed
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_new_error_free(err: *mut std::ffi::c_char) {
    if err.is_null() {
        return;
    }
    drop(std::ffi::CString::from_raw(err))
}

/// Free `ExtismPlugin`
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_free(plugin: *mut Plugin) {
    if plugin.is_null() {
        return;
    }

    let plugin = Box::from_raw(plugin);
    trace!(plugin = plugin.id.to_string(), "called extism_plugin_free");
    drop(plugin)
}

/// Get handle for plugin cancellation
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_cancel_handle(plugin: *const Plugin) -> *const CancelHandle {
    if plugin.is_null() {
        return std::ptr::null();
    }
    let plugin = &*plugin;
    trace!(
        plugin = plugin.id.to_string(),
        "called extism_plugin_cancel_handle"
    );
    &plugin.cancel_handle as *const _
}

/// Cancel a running plugin
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_cancel(handle: *const CancelHandle) -> bool {
    let handle = &*handle;
    trace!(
        plugin = handle.id.to_string(),
        "called extism_plugin_cancel"
    );
    handle.cancel().is_ok()
}

/// Update plugin config values.
//
// This will merge with the existing values, if an existing value is set to `null` it will
// be removed
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

    trace!(
        plugin = plugin.id.to_string(),
        "call to extism_plugin_config with pointer {:?}",
        json
    );
    let data = std::slice::from_raw_parts(json, json_size as usize);
    let json: std::collections::BTreeMap<String, Option<String>> =
        match serde_json::from_slice(data) {
            Ok(x) => x,
            Err(_) => {
                return false;
            }
        };

    let id = plugin.id;
    let config = &mut plugin.current_plugin_mut().manifest.config;
    for (k, v) in json.into_iter() {
        match v {
            Some(v) => {
                trace!(plugin = id.to_string(), "config, adding {k}");
                config.insert(k, v);
            }
            None => {
                trace!(plugin = id.to_string(), "config, removing {k}");
                config.remove(&k);
            }
        }
    }

    let _ = plugin.clear_error();
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
    let name = std::ffi::CStr::from_ptr(func_name);
    trace!(
        plugin = plugin.id.to_string(),
        "extism_plugin_function_exists: {:?}",
        name
    );

    let name = match name.to_str() {
        Ok(x) => x,
        Err(_) => {
            return false;
        }
    };

    let _ = plugin.clear_error();
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
    extism_plugin_call_with_host_context(plugin, func_name, data, data_len, std::ptr::null_mut())
}

#[derive(Clone)]
#[repr(transparent)]
struct CVoidContainer(*mut std::ffi::c_void);

// "You break it, you buy it."
unsafe impl Send for CVoidContainer {}
unsafe impl Sync for CVoidContainer {}

/// Call a function with host context.
///
/// `func_name`: is the function to call
/// `data`: is the input data
/// `data_len`: is the length of `data`
/// `host_context`: a pointer to context data that will be available in host functions
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_call_with_host_context(
    plugin: *mut Plugin,
    func_name: *const c_char,
    data: *const u8,
    data_len: Size,
    host_context: *mut std::ffi::c_void,
) -> i32 {
    if plugin.is_null() {
        return -1;
    }
    let plugin = &mut *plugin;
    let lock = plugin.instance.clone();
    let mut lock = lock.lock().unwrap();

    // Get function name
    let name = std::ffi::CStr::from_ptr(func_name);
    let name = match name.to_str() {
        Ok(name) => name,
        Err(e) => {
            plugin.error_msg = Some(make_error_msg(e.to_string()));
            return -1;
        }
    };

    trace!(
        plugin = plugin.id.to_string(),
        "calling function {} using extism_plugin_call",
        name
    );
    let input = std::slice::from_raw_parts(data, data_len as usize);
    let r = if host_context.is_null() {
        None
    } else {
        Some(CVoidContainer(host_context))
    };
    let res = plugin.raw_call(&mut lock, name, input, r);
    match res {
        Err((e, rc)) => {
            plugin.error_msg = Some(make_error_msg(e.to_string()));
            rc
        }
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

    if plugin.output.error_offset == 0 {
        if let Some(err) = &plugin.error_msg {
            return err.as_ptr() as *const _;
        }
        trace!(plugin = plugin.id.to_string(), "error is NULL");
        return std::ptr::null();
    }

    let offs = plugin.output.error_offset;

    let ptr = plugin.current_plugin_mut().memory_ptr().add(offs as usize) as *const _;

    let len = plugin
        .current_plugin_mut()
        .memory_length(offs)
        .unwrap_or_default();

    let mut data = std::slice::from_raw_parts(ptr, len as usize).to_vec();
    data.push(0);
    plugin.error_msg = Some(data);
    plugin.error_msg.as_ref().unwrap().as_ptr() as *const _
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
    trace!(
        plugin = plugin.id.to_string(),
        "extism_plugin_output_data: offset={}, length={}",
        plugin.output.offset,
        plugin.output.length
    );

    let ptr = plugin.current_plugin_mut().memory_ptr();
    ptr.add(plugin.output.offset as usize)
}

/// Set log file and level.
/// The log level can be either one of: info, error, trace, debug, warn or a more
/// complex filter like `extism=trace,cranelift=debug`
/// The file will be created if it doesn't exist.
#[no_mangle]
pub unsafe extern "C" fn extism_log_file(
    filename: *const c_char,
    log_level: *const c_char,
) -> bool {
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

    set_log_file(file, level).is_ok()
}

// Set the log file Extism will use, this is a global configuration
fn set_log_file(log_file: impl Into<std::path::PathBuf>, filter: &str) -> Result<(), Error> {
    let log_file = log_file.into();
    let s = log_file.to_str();
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

    let res = if s == Some("-") || s == Some("stderr") {
        cfg.with_ansi(true).with_writer(std::io::stderr).try_init()
    } else if s == Some("stdout") {
        cfg.with_ansi(true).with_writer(std::io::stdout).try_init()
    } else {
        let log_file = log_file.to_path_buf();
        let f = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(log_file)
            .expect("Open log file");
        cfg.with_ansi(false)
            .with_writer(move || f.try_clone().unwrap())
            .try_init()
    };

    if let Err(e) = res {
        return Err(Error::msg(e.to_string()));
    }
    Ok(())
}

static LOG_BUFFER: std::sync::Mutex<Option<LogBuffer>> = std::sync::Mutex::new(None);

/// Enable a custom log handler, this will buffer logs until `extism_log_drain` is called
/// Log level should be one of: info, error, trace, debug, warn
#[no_mangle]
pub unsafe extern "C" fn extism_log_custom(log_level: *const c_char) -> bool {
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

    set_log_buffer(level).is_ok()
}

unsafe fn set_log_buffer(filter: &str) -> Result<(), Error> {
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
    *LOG_BUFFER.lock().unwrap() = Some(LogBuffer::default());
    let buf = LOG_BUFFER.lock().unwrap().clone().unwrap();
    cfg.with_ansi(false)
        .with_writer(move || buf.clone())
        .try_init()
        .map_err(|x| Error::msg(x.to_string()))?;
    Ok(())
}

#[no_mangle]
/// Calls the provided callback function for each buffered log line.
/// This is only needed when `extism_log_custom` is used.
pub unsafe extern "C" fn extism_log_drain(handler: ExtismLogDrainFunctionType) {
    if let Some(buf) = LOG_BUFFER.lock().unwrap().as_mut() {
        if let Ok(mut buf) = buf.buffer.lock() {
            for (line, len) in buf.drain(..) {
                handler(line.as_ptr(), len as u64);
            }
        }
    }
}

#[derive(Default, Clone)]
struct LogBuffer {
    buffer:
        std::sync::Arc<std::sync::Mutex<std::collections::VecDeque<(std::ffi::CString, usize)>>>,
}

unsafe impl Send for LogBuffer {}
unsafe impl Sync for LogBuffer {}

impl std::io::Write for LogBuffer {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Ok(s) = std::str::from_utf8(buf) {
            if let Ok(mut buf) = self.buffer.lock() {
                buf.push_back((std::ffi::CString::new(s)?, s.len()));
            }
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// Reset the Extism runtime, this will invalidate all allocated memory
#[no_mangle]
pub unsafe extern "C" fn extism_plugin_reset(plugin: *mut Plugin) -> bool {
    let plugin = &mut *plugin;

    if let Err(e) = plugin.reset() {
        error!(
            plugin = plugin.id.to_string(),
            "unable to reset plugin: {}",
            e.to_string()
        );
        if let Err(e) = plugin.current_plugin_mut().set_error(e.to_string()) {
            error!(
                plugin = plugin.id.to_string(),
                "unable to set error after failed plugin reset: {}",
                e.to_string()
            );
        }
        false
    } else {
        true
    }
}

/// Get the Extism version string
#[no_mangle]
pub unsafe extern "C" fn extism_version() -> *const c_char {
    VERSION.as_ptr() as *const _
}
