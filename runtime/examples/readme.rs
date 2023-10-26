use extism::*;

// pretend this is redis or something :)
type KVStore = std::collections::BTreeMap<String, Vec<u8>>;

// When a first argument separated with a semicolon is provided to `host_fn` it is used as the
// variable name and type for the `UserData` parameter
host_fn!(kv_read(user_data: KVStore; key: String) -> u32 {
    let kv = user_data.get()?;
    let kv = kv.lock().unwrap();
    let value = kv
        .get(&key)
        .map(|x| u32::from_le_bytes(x.clone().try_into().unwrap()))
        .unwrap_or_else(|| 0u32);
    Ok(value)
});

host_fn!(kv_write(user_data: KVStore; key: String, value: u32) {
    let kv = user_data.get()?;
    let mut kv = kv.lock().unwrap();
    kv.insert(key, value.to_le_bytes().to_vec());
    Ok(())
});

fn main() {
    let kv_store = UserData::new(KVStore::default());

    let url = Wasm::url(
        "https://github.com/extism/plugins/releases/latest/download/count_vowels_kvstore.wasm",
    );
    let manifest = Manifest::new([url]);
    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .with_function(
            "kv_read",
            [ValType::I64],
            [ValType::I64],
            kv_store.clone(),
            kv_read,
        )
        .with_function(
            "kv_write",
            [ValType::I64, ValType::I64],
            [],
            kv_store.clone(),
            kv_write,
        )
        .build()
        .unwrap();

    for _ in 0..5 {
        let res = plugin
            .call::<&str, &str>("count_vowels", "Hello, world!")
            .unwrap();
        println!("{}", res);
    }
}
