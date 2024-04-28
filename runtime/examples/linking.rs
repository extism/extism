use extism::*;

fn main() {
    let manifest = Manifest::new([
        // upper.wat provides an export called `host_reflect` that takes a string
        // and returns the same string uppercased
        Wasm::File {
            // See https://github.com/extism/plugins/blob/main/upper.wat
            path: "../wasm/upper.wasm".into(),
            meta: WasmMetadata {
                name: Some("extism:host/user".to_string()),
                hash: None,
            },
        },
        // reflect expects host_reflect to be imported: https://github.com/extism/plugins/blob/e5578bbbdd87f9936a0a8d36df629768b2eff6bb/reflect/src/lib.rs#L5
        // Extism will link the export from upper.wat to the import of reflect.rs at runtime so it
        // can call it
        Wasm::File {
            // See https://github.com/extism/plugins/tree/main/reflect
            path: "../wasm/reflect.wasm".into(),
            meta: WasmMetadata {
                name: Some("main".to_string()),
                hash: None,
            },
        },
    ]);
    let mut plugin = PluginBuilder::new(manifest).build().unwrap();

    for _ in 0..5 {
        let res = plugin
            .call::<&str, &str>("reflect", "Hello, world!")
            .unwrap();
        println!("{}", res);
    }
}
