use extism::*;

fn main() {
    let manifest = Manifest::new([
        Wasm::File {
            // See https://github.com/extism/plugins/blob/main/upper.wat
            path: "../wasm/upper.wasm".into(),
            meta: WasmMetadata {
                name: Some("extism:host/user".to_string()),
                hash: None,
            },
        },
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
