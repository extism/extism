use crate::*;

const WASM_EMPTY: &[u8] = include_bytes!("../../../wasm/empty.wasm");
const WASM_UNREACHABLE: &[u8] = include_bytes!("../../../wasm/unreachable.wasm");

// https://github.com/extism/extism/issues/620
#[test]
fn test_issue_620() {
    // Load and build plugin
    let url = Wasm::data(WASM_EMPTY);
    let manifest = Manifest::new([url]);
    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .build()
        .unwrap();
    // Call test method, this does not work
    let p = plugin.call::<(), String>("test", ()).unwrap();

    println!("{p}");
}

// https://github.com/extism/extism/issues/619
host_fn!(
    _resolve_file_path(path: &str) -> String {
        let path = std::path::PathBuf::from(path);
        let path = path.canonicalize()?;
        Ok(path.display().to_string())
    }
);

// https://github.com/extism/extism/issues/775
#[test]
fn test_issue_775() {
    // Load and build plugin
    let url = Wasm::data(WASM_UNREACHABLE);
    let manifest = Manifest::new([url]);
    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .build()
        .unwrap();
    // Call test method
    let lock = plugin.instance.clone();
    let mut lock = lock.lock().unwrap();
    let res = plugin.raw_call(&mut lock, "do_unreachable", b"", None::<()>);
    let p = match res {
        Err(e) => {
            if e.1 == 0 {
                Err(e.1)
            } else {
                Ok(e.1)
            }
        }
        Ok(code) => Err(code),
    }
    .unwrap();
    println!("{p}");
}

// A plugin that reports an output range larger than its memory should get an error,
// it must not be able to crash the host.
#[test]
fn test_output_set_invalid_length() {
    let wat = |len: i64| {
        format!(
            r#"(module
                (import "extism:host/env" "alloc" (func $alloc (param i64) (result i64)))
                (import "extism:host/env" "output_set" (func $output_set (param i64 i64)))
                (func (export "run") (result i32)
                    (local $o i64)
                    (local.set $o (call $alloc (i64.const 16)))
                    (call $output_set (local.get $o) (i64.const {len}))
                    (i32.const 0)))"#
        )
    };

    // A valid output range still works
    let mut plugin = Plugin::new(Manifest::new([Wasm::data(wat(16))]), [], true).unwrap();
    let out: Vec<u8> = plugin.call("run", "").unwrap();
    assert_eq!(out.len(), 16);

    // An output length of u64::MAX is rejected
    let mut plugin = Plugin::new(Manifest::new([Wasm::data(wat(-1))]), [], true).unwrap();
    let res: Result<Vec<u8>, Error> = plugin.call("run", "");
    assert!(res.map(|x| x.is_empty()).unwrap_or(true));
}
