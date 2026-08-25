use extism::*;

fn copy_input_get_wat() -> &'static str {
    r#"(module
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
    )"#
}

fn put_out_wat() -> &'static str {
    r#"(module
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
    )"#
}

#[test]
fn host_seeds_plugin_reads() {
    let store = DurableStore::memory("seed-1").unwrap();
    store.fs_put("/hello", b"from-host").unwrap();
    let mut plugin = PluginBuilder::new(copy_input_get_wat().as_bytes())
        .with_store(store)
        .build()
        .unwrap();
    let out: Vec<u8> = plugin.call("run", "/hello").unwrap();
    assert_eq!(out, b"from-host");
}

#[test]
fn plugin_writes_host_reads() {
    let store = DurableStore::memory("put-1").unwrap();
    let mut plugin = PluginBuilder::new(put_out_wat().as_bytes())
        .with_store(store.clone())
        .build()
        .unwrap();
    plugin.call::<_, ()>("run", "plugin-bytes").unwrap();
    assert_eq!(
        store.fs_get("/out").unwrap().as_deref(),
        Some(&b"plugin-bytes"[..])
    );
}

#[test]
fn readonly_seed_rejects_guest_put() {
    let store = DurableStore::memory("ro-1").unwrap();
    store.fs_put_ro("/out", b"locked").unwrap();
    let mut plugin = PluginBuilder::new(put_out_wat().as_bytes())
        .with_store(store)
        .build()
        .unwrap();
    let err = plugin.call::<_, ()>("run", "nope").unwrap_err();
    let msg = err.to_string();
    assert!(
        msg.contains("read-only") || msg.contains("wasm") || msg.contains("trap"),
        "{msg}"
    );
}

#[test]
fn missing_store_errors_on_import_call() {
    let mut plugin = PluginBuilder::new(copy_input_get_wat().as_bytes())
        .build()
        .unwrap();
    assert!(plugin.call::<_, Vec<u8>>("run", "/x").is_err());
}

fn copy_input_exec_wat() -> &'static str {
    r#"(module
        (import "extism:host/env" "input_length" (func $ilen (result i64)))
        (import "extism:host/env" "input_load_u8" (func $ild (param i64) (result i32)))
        (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
        (import "extism:host/env" "store_u8" (func $su8 (param i64 i32)))
        (import "extism:host/env" "output_set" (func $out (param i64 i64)))
        (import "extism:host/env" "length" (func $len (param i64) (result i64)))
        (import "extism:host/store" "exec" (func $exec (param i64 i64) (result i64)))
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
            (local.set $h (call $exec (local.get $p) (i64.const 0)))
            (call $out (local.get $h) (call $len (local.get $h))))
    )"#
}

fn id_wat() -> &'static str {
    r#"(module
        (import "extism:host/env" "output_set" (func $out (param i64 i64)))
        (import "extism:host/env" "length" (func $len (param i64) (result i64)))
        (import "extism:host/store" "id" (func $id (result i64)))
        (func (export "run")
            (local $h i64)
            (local.set $h (call $id))
            (call $out (local.get $h) (call $len (local.get $h))))
    )"#
}

fn copy_input_list_wat() -> &'static str {
    r#"(module
        (import "extism:host/env" "input_length" (func $ilen (result i64)))
        (import "extism:host/env" "input_load_u8" (func $ild (param i64) (result i32)))
        (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
        (import "extism:host/env" "store_u8" (func $su8 (param i64 i32)))
        (import "extism:host/env" "output_set" (func $out (param i64 i64)))
        (import "extism:host/env" "length" (func $len (param i64) (result i64)))
        (import "extism:host/store" "list" (func $list (param i64) (result i64)))
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
            (local.set $h (call $list (local.get $p)))
            (call $out (local.get $h) (call $len (local.get $h))))
    )"#
}

#[test]
fn plugin_exec_and_id() {
    let store = DurableStore::memory("exec-1").unwrap();
    let mut plugin = PluginBuilder::new(copy_input_exec_wat().as_bytes())
        .with_store(store.clone())
        .build()
        .unwrap();
    let out: Vec<u8> = plugin.call("run", "SELECT 1 AS n").unwrap();
    let v: serde_json::Value = serde_json::from_slice(&out).unwrap();
    assert_eq!(v["ok"], true);
    assert_eq!(v["rows"][0]["n"], 1);

    let mut plugin = PluginBuilder::new(id_wat().as_bytes())
        .with_store(store)
        .build()
        .unwrap();
    let out: Vec<u8> = plugin.call("run", "").unwrap();
    assert_eq!(out, b"exec-1");
}

#[test]
fn plugin_list_paths() {
    let store = DurableStore::memory("list-1").unwrap();
    store.fs_put("/a/x", b"1").unwrap();
    store.fs_put("/a/y", b"2").unwrap();
    store.fs_put("/b", b"3").unwrap();
    let mut plugin = PluginBuilder::new(copy_input_list_wat().as_bytes())
        .with_store(store)
        .build()
        .unwrap();
    let out: Vec<u8> = plugin.call("run", "/a").unwrap();
    let listed: Vec<String> = serde_json::from_slice(&out).unwrap();
    assert_eq!(listed, vec!["/a/x".to_string(), "/a/y".to_string()]);
}

#[test]
fn durable_store_handle_is_shared() {
    let store = DurableStore::memory("shared-1").unwrap();
    store.fs_put("/k", b"v").unwrap();
    let plugin = PluginBuilder::new(id_wat().as_bytes())
        .with_store(store.clone())
        .build()
        .unwrap();
    assert_eq!(plugin.durable_store().unwrap().id(), "shared-1");
    assert_eq!(
        plugin
            .durable_store()
            .unwrap()
            .fs_get("/k")
            .unwrap()
            .as_deref(),
        Some(&b"v"[..])
    );
}
