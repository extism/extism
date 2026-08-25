//! Manifest-style combining: name a helper module, import its exports from main.
//!
//! Extism's Manifest does not share linear memories between plugins. It puts
//! each Wasm in a map keyed by `WasmMetadata.name` (or `main` for the last
//! module) and lets Wasmtime's linker satisfy `(import "that-name" "export")`.
//! Wasm3 v0.9.0 does the same for **functions** via `m3_SetModuleName` +
//! `ResolveImportedFunction`.

use wasm3_sys::{Environment, Runtime};

fn wat(src: &str) -> Vec<u8> {
    wat::parse_str(src).unwrap()
}

/// `linking.rs` / `upper.wasm` shape: helper named `extism:host/user`, main imports it.
#[test]
fn manifest_named_function_import() {
    let helper = wat(r#"(module
            (func (export "host_reflect") (param i32) (result i32)
                local.get 0)
        )"#);
    let main = wat(r#"(module
            (import "extism:host/user" "host_reflect"
                (func $host_reflect (param i32) (result i32)))
            (func (export "reflect") (param i32) (result i32)
                local.get 0
                call $host_reflect)
        )"#);

    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    {
        let mut helper = rt.parse_and_load(&helper).unwrap();
        helper.set_name("extism:host/user").unwrap();
        helper.compile().unwrap();
    }
    rt.parse_and_load(&main).unwrap();

    let reflect = rt.find_function("reflect").unwrap();
    assert_eq!(reflect.call_i32(&[7]).unwrap(), 7);
}

/// `test_linking` commander/main shape: helper keeps state in a Wasm global,
/// not linear memory. Main calls into it. Both talk to the kernel in the real
/// SDK; here we only need the cross-module calls.
#[test]
fn helper_state_in_global() {
    let commander = wat(r#"(module
            (global $counter (mut i32) (i32.const 0))
            (func (export "_start")
                (global.set $counter (i32.add (global.get $counter) (i32.const 1))))
            (func (export "read_counter") (result i32)
                (global.get $counter))
        )"#);
    let main = wat(r#"(module
            (import "commander" "_start" (func $start))
            (import "commander" "read_counter" (func $read (result i32)))
            (func (export "run") (result i32)
                (call $start)
                (call $start)
                (call $start)
                (call $start)
                (call $read))
        )"#);

    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    {
        let mut commander = rt.parse_and_load(&commander).unwrap();
        commander.set_name("commander").unwrap();
        commander.compile().unwrap();
    }
    rt.parse_and_load(&main).unwrap();

    let run = rt.find_function("run").unwrap();
    assert_eq!(run.call_i32(&[]).unwrap(), 4);
}

/// Two modules that both declare `(memory 1)` share one buffer in a single
/// `M3Runtime`. A store in A is visible as a load in B. That is the actual
/// Wasm3 limit — not Manifest naming, and not the native kernel.
#[test]
fn two_declared_memories_share_the_runtime_buffer() {
    let writer = wat(r#"(module
            (memory 1)
            (func (export "write") (param i32) (result i32)
                i32.const 0
                local.get 0
                i32.store
                local.get 0)
        )"#);
    let reader = wat(r#"(module
            (memory 1)
            (func (export "read") (result i32)
                i32.const 0
                i32.load)
        )"#);

    let env = Environment::new().unwrap();
    let mut rt = Runtime::new(&env, Runtime::DEFAULT_STACK).unwrap();
    rt.parse_and_load(&writer).unwrap();
    rt.parse_and_load(&reader).unwrap();

    rt.find_function("write")
        .unwrap()
        .call_i32(&[0x1122_3344u32 as i32])
        .unwrap();
    assert_eq!(
        rt.find_function("read").unwrap().call_i32(&[]).unwrap(),
        0x1122_3344u32 as i32
    );
}
