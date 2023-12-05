use crate::*;

const WASM_EMPTY: &[u8] = include_bytes!("../../../wasm/empty.wasm");

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

    println!("{}", p);
}

// https://github.com/extism/extism/issues/619
host_fn!(
    _resolve_file_path(path: &str) -> String {
        let path = std::path::PathBuf::from(path);
        let path = path.canonicalize()?;
        Ok(path.display().to_string())
    }
);
