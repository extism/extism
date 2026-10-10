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

// https://github.com/extism/extism/issues/851
// On platforms without a usable default wasmtime cache config (e.g. Android,
// where no default config path can be determined), creating a plugin with
// default cache settings must not fail: caching is only an optimization, so
// fall back to a disabled cache.
//
// A missing default config file is fine, but a present-but-unusable one makes
// `wasmtime::Cache::from_file(None)` fail the same way an undeterminable
// default path does. An invalid config file therefore stands in for those
// platforms here, since the absence of a home directory cannot be simulated
// on a machine that has one.
#[test]
fn test_issue_851() {
    // Isolated HOME with an invalid default wasmtime cache config. Layouts
    // follow the `directories` crate rules used by wasmtime.
    let home = std::env::temp_dir().join(format!("extism-issue-851-{}", std::process::id()));
    #[cfg(target_os = "macos")]
    let config = home
        .join("Library")
        .join("Application Support")
        .join("BytecodeAlliance.wasmtime")
        .join("config.toml");
    #[cfg(not(target_os = "macos"))]
    let config = home.join(".config").join("wasmtime").join("config.toml");
    std::fs::create_dir_all(config.parent().unwrap()).unwrap();
    std::fs::write(&config, "this is not valid toml =[[[").unwrap();

    // Variables consulted when resolving the default cache config
    const VARS: [&str; 4] = [
        "HOME",
        "XDG_CONFIG_HOME",
        "XDG_CACHE_HOME",
        "EXTISM_CACHE_CONFIG",
    ];
    let saved: Vec<(&str, Option<std::ffi::OsString>)> =
        VARS.iter().map(|k| (*k, std::env::var_os(k))).collect();
    std::env::set_var("HOME", &home);
    std::env::remove_var("XDG_CONFIG_HOME");
    std::env::remove_var("XDG_CACHE_HOME");
    std::env::remove_var("EXTISM_CACHE_CONFIG");

    let res = (|| {
        let url = Wasm::data(WASM_EMPTY);
        let manifest = Manifest::new([url]);
        PluginBuilder::new(manifest).build().map(|_| ())
    })();

    // Restore the environment before asserting
    for (k, v) in saved {
        match v {
            Some(v) => std::env::set_var(k, v),
            None => std::env::remove_var(k),
        }
    }
    std::fs::remove_dir_all(&home).ok();

    res.expect("plugin creation should succeed without a usable default cache config");
}

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
