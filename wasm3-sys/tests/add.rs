use wasm3_sys::{Environment, Runtime};

fn add_wasm() -> Vec<u8> {
    wat::parse_str(
        r#"(module
            (func (export "add") (param i32 i32) (result i32)
                local.get 0
                local.get 1
                i32.add)
        )"#,
    )
    .unwrap()
}

#[test]
fn version_is_0_9_0() {
    assert_eq!(wasm3_sys::WASM3_VERSION, "v0.9.0");
}

#[test]
fn add_two_i32s() {
    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    rt.parse_and_load(&add_wasm()).unwrap();
    let add = rt.find_function("add").unwrap();
    assert_eq!(add.arg_count(), 2);
    assert_eq!(add.ret_count(), 1);
    assert_eq!(add.call_i32(&[3, 6]).unwrap(), 9);
    assert_eq!(add.call_i32(&[-1, 1]).unwrap(), 0);
}

unsafe extern "C" fn host_double(
    _runtime: wasm3_sys::ffi::IM3Runtime,
    _ctx: wasm3_sys::ffi::IM3ImportContext,
    sp: *mut u64,
    _mem: *mut std::os::raw::c_void,
) -> wasm3_sys::ffi::M3Result {
    let arg = wasm3_sys::raw_arg_i32(sp, 1, 0);
    wasm3_sys::raw_set_i32(sp, arg.wrapping_mul(2));
    std::ptr::null()
}

#[test]
fn host_function_import() {
    let wasm = wat::parse_str(
        r#"(module
            (import "env" "double" (func $double (param i32) (result i32)))
            (func (export "run") (param i32) (result i32)
                local.get 0
                call $double)
        )"#,
    )
    .unwrap();

    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    {
        let mut module = rt.parse_and_load(&wasm).unwrap();
        module
            .link_raw("env", "double", "i(i)", host_double)
            .unwrap();
    }

    let run = rt.find_function("run").unwrap();
    assert_eq!(run.call_i32(&[21]).unwrap(), 42);
}

#[test]
fn missing_export_is_error() {
    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    rt.parse_and_load(&add_wasm()).unwrap();
    assert!(rt.find_function("nope").is_err());
}
