use extism::*;

fn main() {
    let manifest = Manifest::new([
        Wasm::Data {
            data: br#"
                (module
                    (import "wasi_snapshot_preview1" "random_get" (func $random (param i32 i32) (result i32)))
                    (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
                    (import "extism:host/user" "hello" (func $hello))
                    (global $counter (mut i32) (i32.const 0))
                    (func $start (export "_start")
                        (global.set $counter (i32.add (global.get $counter) (i32.const 1)))
                    )
                    (func (export "read_counter") (result i32)
                        (global.get $counter)
                    )
                )
            "#.to_vec(),
            meta: WasmMetadata {
                name: Some("commander".to_string()),
                hash: None,
            },
        },
        Wasm::Data {
            data: br#"
                (module
                    (import "commander" "_start" (func $commander_start))
                    (import "commander" "read_counter" (func $commander_read_counter (result i32)))
                    (import "extism:host/env" "store_u64" (func $store_u64 (param i64 i64)))
                    (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
                    (import "extism:host/user" "hello" (func $hello))
                    (import "extism:host/env" "output_set" (func $output_set (param i64 i64)))
                    (func (export "run") (result i32)
                        (local $output i64)
                        (local.set $output (call $alloc (i64.const 8)))

                        (call $commander_start)
                        (call $commander_start)
                        (call $commander_start)
                        (call $commander_start)
                        (call $hello)
                        (call $store_u64 (local.get $output) (i64.extend_i32_u (call $commander_read_counter)))
                        (call $output_set (local.get $output) (i64.const 8))
                        i32.const 0
                    )
                )
            "#.to_vec(),
            meta: WasmMetadata {
                name: Some("main".to_string()),
                hash: None,
            },
        },
    ]);
    let mut plugin = PluginBuilder::new(manifest).with_wasi(true).with_function("hello", [], [], UserData::new(()), |_, _, _, _| {
        eprintln!("hello!");
        Ok(())
    }).build().unwrap();

    for _ in 0..5 {
        let res = plugin
            .call::<&str, i64>("run", "Hello, world!")
            .unwrap();
        println!("{}", res);
    }
}
