//! Drive `echo.wasm` through Wasm3 + the native Extism kernel.
//!
//! This is the architecture a Wasm3 Extism runtime would use: plugin module in
//! Wasm3, `extism:host/env` implemented as host functions over `kernel::Kernel`.

use extism_runtime_kernel::Kernel;
use std::os::raw::c_void;
use wasm3_sys::ffi::{IM3ImportContext, IM3Runtime, M3Result};
use wasm3_sys::{raw_arg_i32, raw_arg_i64, raw_set_i32, raw_set_i64, Environment, Runtime};

const ECHO: &[u8] = include_bytes!("../../wasm/echo.wasm");

unsafe fn kernel_from(ctx: IM3ImportContext) -> *mut Kernel {
    (*ctx).userdata as *mut Kernel
}

unsafe extern "C" fn env_alloc(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let n = raw_arg_i64(sp, 1, 0) as u64;
    let handle = (*kernel_from(ctx)).alloc(n) as i64;
    raw_set_i64(sp, handle);
    std::ptr::null()
}

unsafe extern "C" fn env_input_length(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    raw_set_i64(sp, (*kernel_from(ctx)).input_length() as i64);
    std::ptr::null()
}

unsafe extern "C" fn env_input_load_u8(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let off = raw_arg_i64(sp, 1, 0) as u64;
    raw_set_i32(sp, (*kernel_from(ctx)).input_load_u8(off) as i32);
    std::ptr::null()
}

unsafe extern "C" fn env_input_load_u64(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let off = raw_arg_i64(sp, 1, 0) as u64;
    raw_set_i64(sp, (*kernel_from(ctx)).input_load_u64(off) as i64);
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
    (*kernel_from(ctx)).store_u8(p, x);
    std::ptr::null()
}

unsafe extern "C" fn env_store_u64(
    _rt: IM3Runtime,
    ctx: IM3ImportContext,
    sp: *mut u64,
    _mem: *mut c_void,
) -> M3Result {
    let p = raw_arg_i64(sp, 0, 0) as u64;
    let x = raw_arg_i64(sp, 0, 1) as u64;
    (*kernel_from(ctx)).store_u64(p, x);
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
    (*kernel_from(ctx)).output_set(p, len);
    std::ptr::null()
}

#[test]
fn echo_through_native_kernel() {
    let mut kernel = Kernel::new();
    let input = b"hello wasm3 v0.9.0";
    let handle = kernel.copy_from_slice(input);
    assert!(handle > 0);
    kernel.input_set(handle, input.len() as u64);

    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    {
        let mut module = rt.parse_and_load(ECHO).unwrap();

        let k = &mut kernel as *mut Kernel as *const c_void;
        let ns = "extism:host/env";
        unsafe {
            module
                .link_raw_ex(ns, "alloc", "I(I)", env_alloc, k)
                .unwrap();
            module
                .link_raw_ex(ns, "input_length", "I()", env_input_length, k)
                .unwrap();
            module
                .link_raw_ex(ns, "input_load_u8", "i(I)", env_input_load_u8, k)
                .unwrap();
            module
                .link_raw_ex(ns, "input_load_u64", "I(I)", env_input_load_u64, k)
                .unwrap();
            module
                .link_raw_ex(ns, "store_u8", "v(Ii)", env_store_u8, k)
                .unwrap();
            module
                .link_raw_ex(ns, "store_u64", "v(II)", env_store_u64, k)
                .unwrap();
            module
                .link_raw_ex(ns, "output_set", "v(II)", env_output_set, k)
                .unwrap();
        }
    }

    rt.find_function("echo").unwrap().call_void().unwrap();

    let out_off = kernel.output_offset() as usize;
    let out_len = kernel.output_length() as usize;
    let output = &kernel.as_slice()[out_off..out_off + out_len];
    assert_eq!(output, input);
}
