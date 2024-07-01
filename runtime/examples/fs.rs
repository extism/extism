use extism::*;
fn main() {
    let url = Wasm::file("D:/x/rust/fs/target/wasm32-wasi/debug/fs.wasm");
    let manifest = Manifest::new([url])
        .with_allowed_path("d:/x/go/fs/data", "/data")
        .with_config_key("path", "/data/data.txt");

    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .build()
        .unwrap();

    println!("trying to read file: ");

    let res = plugin
        .call::<&str, &str>("try_read", "")
        .unwrap();

    println!("{:?}", res);

    println!("-----------------------------------------------------");

    println!("trying to write file: ");
    let line = format!("Hello World at {:?}", std::time::SystemTime::now());
    let res2 = plugin
    .call::<&str, &str>("try_write", &line)
    .unwrap();

    println!("{:?}", res2);

    println!("done!");
}
