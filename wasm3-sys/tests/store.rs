//! Extism 2 store extension on Wasm3: native kernel + `extism:host/store`.
//!
//! Same ABI as the Wasmtime runtime (`runtime/src/store_ext.rs`). The plugin
//! stays in Wasm3; SQLite stays on the host.

use extism_runtime_kernel::Kernel;
use extism_store::DurableStore;
use std::os::raw::c_void;
use wasm3_sys::ffi::{IM3ImportContext, IM3Runtime, M3Result};
use wasm3_sys::{raw_arg_i32, raw_arg_i64, raw_set_i32, raw_set_i64, Environment, Runtime};

struct Host {
    kernel: Kernel,
    store: DurableStore,
}

unsafe fn host(ctx: IM3ImportContext) -> *mut Host {
    (*ctx).userdata as *mut Host
}

fn kernel_bytes(k: &mut Kernel, handle: u64) -> Vec<u8> {
    let len = k.length(handle) as usize;
    if handle == 0 || len == 0 {
        return Vec::new();
    }
    let start = handle as usize;
    k.as_slice()[start..start + len].to_vec()
}

unsafe extern "C" fn env_alloc(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let n = raw_arg_i64(sp, 1, 0) as u64;
    let handle = (*host(ctx)).kernel.alloc(n) as i64;
    raw_set_i64(sp, handle);
    std::ptr::null()
}

unsafe extern "C" fn env_input_length(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    raw_set_i64(sp, (*host(ctx)).kernel.input_length() as i64);
    std::ptr::null()
}

unsafe extern "C" fn env_input_load_u8(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let off = raw_arg_i64(sp, 1, 0) as u64;
    raw_set_i32(sp, (*host(ctx)).kernel.input_load_u8(off) as i32);
    std::ptr::null()
}

unsafe extern "C" fn env_store_u8(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let p = raw_arg_i64(sp, 0, 0) as u64;
    let x = raw_arg_i32(sp, 0, 1) as u8;
    (*host(ctx)).kernel.store_u8(p, x);
    std::ptr::null()
}

unsafe extern "C" fn env_output_set(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let p = raw_arg_i64(sp, 0, 0) as u64;
    let len = raw_arg_i64(sp, 0, 1) as u64;
    (*host(ctx)).kernel.output_set(p, len);
    std::ptr::null()
}

unsafe extern "C" fn env_length(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let off = raw_arg_i64(sp, 1, 0) as u64;
    raw_set_i64(sp, (*host(ctx)).kernel.length(off) as i64);
    std::ptr::null()
}

unsafe extern "C" fn store_get(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let h = &mut *host(ctx);
    let path_off = raw_arg_i64(sp, 1, 0) as u64;
    let path = String::from_utf8_lossy(&kernel_bytes(&mut h.kernel, path_off)).into_owned();
    h.kernel.free(path_off);
    match h.store.fs_get(&path) {
        Ok(Some(bytes)) => raw_set_i64(sp, h.kernel.copy_from_slice(&bytes) as i64),
        Ok(None) => raw_set_i64(sp, 0),
        Err(_) => return c"store get failed".as_ptr(),
    }
    std::ptr::null()
}

unsafe extern "C" fn store_put(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let h = &mut *host(ctx);
    let path_off = raw_arg_i64(sp, 0, 0) as u64;
    let data_off = raw_arg_i64(sp, 0, 1) as u64;
    let path = String::from_utf8_lossy(&kernel_bytes(&mut h.kernel, path_off)).into_owned();
    let body = kernel_bytes(&mut h.kernel, data_off);
    h.kernel.free(path_off);
    if data_off != 0 {
        h.kernel.free(data_off);
    }
    if h.store.fs_put_guest(&path, &body).is_err() {
        return c"store put failed".as_ptr();
    }
    std::ptr::null()
}

fn wat(src: &str) -> Vec<u8> {
    wat::parse_str(src).unwrap()
}

fn copy_input_get() -> Vec<u8> {
    wat(r#"(module
        (import "extism:host/env" "input_length" (func $ilen (result i64)))
        (import "extism:host/env" "input_load_u8" (func $ild (param i64) (result i32)))
        (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
        (import "extism:host/env" "store_u8" (func $su8 (param i64 i32)))
        (import "extism:host/env" "output_set" (func $out (param i64 i64)))
        (import "extism:host/env" "length" (func $len (param i64) (result i64)))
        (import "extism:host/store" "get" (func $get (param i64) (result i64)))
        (func (export "run")
            (local $n i64) (local $p i64) (local $i i64) (local $h i64)
            (local.set $n (call $ilen))
            (local.set $p (call $alloc (local.get $n)))
            (loop $copy
                (if (i64.lt_u (local.get $i) (local.get $n))
                    (then
                        (call $su8
                            (i64.add (local.get $p) (local.get $i))
                            (call $ild (local.get $i)))
                        (local.set $i (i64.add (local.get $i) (i64.const 1)))
                        (br $copy))))
            (local.set $h (call $get (local.get $p)))
            (call $out (local.get $h) (call $len (local.get $h))))
    )"#)
}

fn put_out() -> Vec<u8> {
    wat(r#"(module
        (import "extism:host/env" "input_length" (func $ilen (result i64)))
        (import "extism:host/env" "input_load_u8" (func $ild (param i64) (result i32)))
        (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
        (import "extism:host/env" "store_u8" (func $su8 (param i64 i32)))
        (import "extism:host/store" "put" (func $put (param i64 i64)))
        (func (export "run")
            (local $n i64) (local $data i64) (local $i i64) (local $path i64)
            (local.set $n (call $ilen))
            (local.set $data (call $alloc (local.get $n)))
            (loop $copy
                (if (i64.lt_u (local.get $i) (local.get $n))
                    (then
                        (call $su8
                            (i64.add (local.get $data) (local.get $i))
                            (call $ild (local.get $i)))
                        (local.set $i (i64.add (local.get $i) (i64.const 1)))
                        (br $copy))))
            (local.set $path (call $alloc (i64.const 4)))
            (call $su8 (local.get $path) (i32.const 47))
            (call $su8 (i64.add (local.get $path) (i64.const 1)) (i32.const 111))
            (call $su8 (i64.add (local.get $path) (i64.const 2)) (i32.const 117))
            (call $su8 (i64.add (local.get $path) (i64.const 3)) (i32.const 116))
            (call $put (local.get $path) (local.get $data)))
    )"#)
}

fn link_ok(result: wasm3_sys::Result<()>) {
    match result {
        Ok(()) => {}
        Err(e) if e.to_string().contains("function lookup failed") => {}
        Err(e) => panic!("{e}"),
    }
}

unsafe fn link_env_and_store(module: &mut wasm3_sys::LoadedModule<'_>, userdata: *const c_void) {
    let env = "extism:host/env";
    let store = extism_store::STORE_MODULE;
    link_ok(module.link_raw_ex(env, "alloc", "I(I)", env_alloc, userdata));
    link_ok(module.link_raw_ex(env, "input_length", "I()", env_input_length, userdata));
    link_ok(module.link_raw_ex(env, "input_load_u8", "i(I)", env_input_load_u8, userdata));
    link_ok(module.link_raw_ex(env, "store_u8", "v(Ii)", env_store_u8, userdata));
    link_ok(module.link_raw_ex(env, "output_set", "v(II)", env_output_set, userdata));
    link_ok(module.link_raw_ex(env, "length", "I(I)", env_length, userdata));
    link_ok(module.link_raw_ex(store, "get", "I(I)", store_get, userdata));
    link_ok(module.link_raw_ex(store, "put", "v(II)", store_put, userdata));
}

fn run(wasm: &[u8], host: &mut Host, input: &[u8]) {
    host.kernel.reset();
    let handle = host.kernel.copy_from_slice(input);
    host.kernel.input_set(handle, input.len() as u64);

    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    {
        let mut module = rt.parse_and_load(wasm).unwrap();
        let userdata = host as *mut Host as *const c_void;
        unsafe { link_env_and_store(&mut module, userdata) };
    }
    rt.find_function("run").unwrap().call_void().unwrap();
}

#[test]
fn wasm3_host_seeds_plugin_reads() {
    let mut host = Host {
        kernel: Kernel::new(),
        store: DurableStore::memory("wasm3-seed").unwrap(),
    };
    host.store.fs_put("/hello", b"from-host").unwrap();
    run(&copy_input_get(), &mut host, b"/hello");
    let out_off = host.kernel.output_offset() as usize;
    let out_len = host.kernel.output_length() as usize;
    assert_eq!(
        &host.kernel.as_slice()[out_off..out_off + out_len],
        b"from-host"
    );
}

#[test]
fn wasm3_plugin_writes_host_reads() {
    let mut host = Host {
        kernel: Kernel::new(),
        store: DurableStore::memory("wasm3-put").unwrap(),
    };
    run(&put_out(), &mut host, b"plugin-bytes");
    assert_eq!(
        host.store.fs_get("/out").unwrap().as_deref(),
        Some(&b"plugin-bytes"[..])
    );
}
