use extism::*;
fn main() {
    let url = Wasm::file("../wasm/read_write.wasm");
    let manifest = Manifest::new([url])
        // This will fail because we're using a readonly path (specified with the `ro:` prefix)
        // to overwrite the data file, remove `ro:` from the path on the following line
        .with_allowed_path("ro:src/tests/data".to_string(), "/data")
        .with_config_key("path", "/data/data.txt");

    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .build()
        .unwrap();

    println!("trying to read file: ");

    let res = plugin.call::<&str, &str>("try_read", "").unwrap();

    println!("{:?}", res);

    println!("-----------------------------------------------------");

    // If the allowed path is readonly then writing back to the file should fail
    println!("trying to write file: ");
    let line = format!(
        "Hello World at {:?}\n",
        std::time::SystemTime::now()
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .unwrap()
    );
    let res2 = plugin.call::<&str, &str>("try_write", &line).unwrap();

    println!("{:?}", res2);

    println!("done!");
}
