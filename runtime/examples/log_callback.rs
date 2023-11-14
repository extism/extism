use extism::*;

static LOGS: std::sync::Mutex<Vec<String>> = std::sync::Mutex::new(Vec::new());

fn handle_logs(msg: &str) {
    LOGS.lock().unwrap().push(msg.to_string())
}

fn main() {
    set_log_callback(handle_logs, "extism=trace").unwrap();
    let url = Wasm::file("../wasm/code.wasm");
    let manifest = Manifest::new([url]);
    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .build()
        .unwrap();

    for _ in 0..5 {
        let res = plugin
            .call::<&str, &str>("count_vowels", "Hello, world!")
            .unwrap();
        tracing::info!("{}", res);
    }

    println!("Dumping logs");

    for line in LOGS.lock().unwrap().iter() {
        print!("{}", line);
    }
}
