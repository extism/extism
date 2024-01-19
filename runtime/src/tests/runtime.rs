use crate::*;
use std::{io::Write, time::Instant};

const WASM: &[u8] = include_bytes!("../../../wasm/code-functions.wasm");
const WASM_NO_FUNCTIONS: &[u8] = include_bytes!("../../../wasm/code.wasm");
const WASM_LOOP: &[u8] = include_bytes!("../../../wasm/loop.wasm");
const WASM_GLOBALS: &[u8] = include_bytes!("../../../wasm/globals.wasm");
const WASM_REFLECT: &[u8] = include_bytes!("../../../wasm/reflect.wasm");
const WASM_HTTP: &[u8] = include_bytes!("../../../wasm/http.wasm");

host_fn!(pub hello_world (a: String) -> String { Ok(a) });

// Which is the same as:
// fn hello_world(
//     plugin: &mut CurrentPlugin,
//     inputs: &[Val],
//     outputs: &mut [Val],
//     _user_data: UserData<()>,
// ) -> Result<(), Error> {
//     let input: String = plugin.memory_get_val(&inputs[0]).unwrap();
//     let output = plugin.memory_new(&input).unwrap();
//     outputs[0] = plugin.memory_to_val(output);
//     Ok(())
// }

fn hello_world_panic(
    _plugin: &mut CurrentPlugin,
    _inputs: &[Val],
    _outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    panic!("This should not run");
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
pub struct Count {
    count: usize,
}

#[test]
fn it_works() {
    let log = tracing_subscriber::fmt()
        .with_ansi(false)
        .with_env_filter("extism=debug")
        .with_writer(std::fs::File::create("test.log").unwrap())
        .try_init()
        .is_ok();

    let wasm_start = Instant::now();

    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world,
    )
    .with_namespace(EXTISM_USER_MODULE);
    let g = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world_panic,
    )
    .with_namespace("test");

    let mut plugin = Plugin::new(WASM, [f, g], true).unwrap();
    println!("register loaded plugin: {:?}", wasm_start.elapsed());

    let repeat = 1182;
    let input = "aeiouAEIOU____________________________________&smtms_y?".repeat(repeat);
    let Json(count) = plugin
        .call::<_, Json<Count>>("count_vowels", &input)
        .unwrap();

    assert_eq!(
        count,
        Count { count: 11820 },
        "expecting vowel count of {}, but got {}",
        10 * repeat,
        count.count,
    );

    println!(
        "register plugin + function call: {:?}, sent input size: {} bytes",
        wasm_start.elapsed(),
        input.len()
    );

    println!("--------------");

    let mut test_times = vec![];
    for _ in 0..100 {
        let test_start = Instant::now();
        plugin.call::<_, &[u8]>("count_vowels", &input).unwrap();
        test_times.push(test_start.elapsed());
    }

    let native_test = || {
        let native_start = Instant::now();
        let mut _native_vowel_count = 0;
        let input: &[u8] = input.as_ref();
        for i in input {
            let i = *i;
            if i == b'A'
                || i == b'E'
                || i == b'I'
                || i == b'O'
                || i == b'U'
                || i == b'a'
                || i == b'e'
                || i == b'i'
                || i == b'o'
                || i == b'u'
            {
                _native_vowel_count += 1;
            }
        }
        native_start.elapsed()
    };

    let native_test_times = (0..100).map(|_| native_test());
    let native_num_tests = native_test_times.len();

    let native_sum: std::time::Duration = native_test_times
        .into_iter()
        .reduce(|accum: std::time::Duration, elapsed| accum + elapsed)
        .unwrap();
    let native_avg: std::time::Duration = native_sum / native_num_tests as u32;

    println!(
        "native function call (avg, N = {}): {:?}",
        native_num_tests, native_avg
    );

    let num_tests = test_times.len();
    let sum: std::time::Duration = test_times
        .into_iter()
        .reduce(|accum: std::time::Duration, elapsed| accum + elapsed)
        .unwrap();
    let avg: std::time::Duration = sum / num_tests as u32;

    println!("wasm function call (avg, N = {}): {:?}", num_tests, avg);

    // Check that log file was written to
    if log {
        let meta = std::fs::metadata("test.log").unwrap();
        assert!(meta.len() > 0);
    }
}

#[test]
fn test_plugin_threads() {
    let p = std::sync::Arc::new(std::sync::Mutex::new(
        PluginBuilder::new(WASM)
            .with_function(
                "hello_world",
                [PTR],
                [PTR],
                UserData::default(),
                hello_world,
            )
            .with_wasi(true)
            .build()
            .unwrap(),
    ));

    let mut threads = vec![];
    for _ in 0..3 {
        let plugin = p.clone();
        let a = std::thread::spawn(move || {
            let mut plugin = plugin.lock().unwrap();
            for _ in 0..10 {
                let Json(count) = plugin
                    .call::<_, Json<Count>>("count_vowels", "this is a test aaa")
                    .unwrap();
                assert_eq!(Count { count: 7 }, count);
            }
        });
        threads.push(a);
    }
    for thread in threads {
        thread.join().unwrap();
    }
}

#[test]
fn test_cancel() {
    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world,
    );

    let mut plugin = Plugin::new(WASM_LOOP, [f], true).unwrap();
    let handle = plugin.cancel_handle();

    for _ in 0..5 {
        let start = std::time::Instant::now();
        let h = handle.clone();
        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_secs(1));
            assert!(h.cancel().is_ok());
        });
        let _output: Result<&[u8], Error> = plugin.call("loop_forever", "abc123");
        let end = std::time::Instant::now();
        let time = end - start;
        println!("Cancelled plugin ran for {:?}", time);
    }
}

#[test]
fn test_timeout() {
    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world,
    );

    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM_LOOP)])
        .with_timeout(std::time::Duration::from_secs(1));
    let mut plugin = Plugin::new(manifest, [f], true).unwrap();

    let start = std::time::Instant::now();
    let output: Result<&[u8], Error> = plugin.call("loop_forever", "abc123");
    let end = std::time::Instant::now();
    let time = end - start;
    let err = output.unwrap_err().root_cause().to_string();
    println!(
        "Timed out plugin ran for {:?}, with error: {:?}",
        time, &err
    );
    assert!(err == "timeout");
}

typed_plugin!(pub TestTypedPluginGenerics {
    count_vowels<T: FromBytes<'a>>(&str) -> T
});

typed_plugin!(CountVowelsPlugin {
    count_vowels(&str) -> Json<Count>;
});

#[test]
fn test_typed_plugin_macro() {
    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world,
    );

    let mut plugin: CountVowelsPlugin = Plugin::new(WASM, [f], true).unwrap().try_into().unwrap();

    let Json(output0): Json<Count> = plugin.count_vowels("abc123").unwrap();
    let Json(output1): Json<Count> = plugin.0.call("count_vowels", "abc123").unwrap();

    assert_eq!(output0, output1)
}

#[test]
fn test_multiple_instantiations() {
    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world,
    );

    let mut plugin: CountVowelsPlugin = Plugin::new(WASM, [f], true).unwrap().try_into().unwrap();

    // This is 10,001 because the wasmtime store limit is 10,000 - we want to test
    // that our reinstantiation process is working and that limit is never hit.
    for _ in 0..10001 {
        let _output: Json<Count> = plugin.count_vowels("abc123").unwrap();
    }
}

#[test]
fn test_globals() {
    let mut plugin = Plugin::new(WASM_GLOBALS, [], true).unwrap();
    for i in 0..100000 {
        let Json(count) = plugin.call::<_, Json<Count>>("globals", "").unwrap();
        assert_eq!(count.count, i);
    }
}

#[test]
fn test_toml_manifest() {
    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM_NO_FUNCTIONS)])
        .with_timeout(std::time::Duration::from_secs(1));

    let manifest_toml = toml::to_string_pretty(&manifest).unwrap();
    let mut plugin = Plugin::new(manifest_toml.as_bytes(), [], true).unwrap();

    let output = plugin.call("count_vowels", "abc123").unwrap();
    let count: serde_json::Value = serde_json::from_slice(output).unwrap();
    assert_eq!(count.get("count").unwrap().as_i64().unwrap(), 1);
}

#[test]
fn test_fuzz_reflect_plugin() {
    // assert!(set_log_file("stdout", Some(log::Level::Trace)));
    let f = Function::new(
        "host_reflect",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world,
    );

    let mut plugin = Plugin::new(WASM_REFLECT, [f], true).unwrap();

    for i in 1..65540 {
        let input = "a".repeat(i);
        let output = plugin.call("reflect", &input);
        let output = std::str::from_utf8(output.unwrap()).unwrap();
        assert_eq!(output, input);
    }
}

#[test]
fn test_memory_max() {
    // Should fail with memory.max set
    let manifest =
        Manifest::new([extism_manifest::Wasm::data(WASM_NO_FUNCTIONS)]).with_memory_max(16);
    let mut plugin = Plugin::new(manifest, [], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(65536 * 2));
    assert!(output.is_err());

    let err = output.unwrap_err().root_cause().to_string();
    println!("{:?}", err);
    assert_eq!(err, "oom");

    // Should pass with memory.max set to a large enough number
    let manifest =
        Manifest::new([extism_manifest::Wasm::data(WASM_NO_FUNCTIONS)]).with_memory_max(17);
    let mut plugin = Plugin::new(manifest, [], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(65536 * 2));
    assert!(output.is_ok());

    // Should pass without it
    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM_NO_FUNCTIONS)]);
    let mut plugin = Plugin::new(manifest, [], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(65536 * 2));
    assert!(output.is_ok());
}

fn hello_world_set_error(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    plugin.set_error("TEST")?;
    outputs[0] = inputs[0].clone();
    Ok(())
}

fn hello_world_set_error_bail(
    plugin: &mut CurrentPlugin,
    _inputs: &[Val],
    _outputs: &mut [Val],
    _user_data: UserData<()>,
) -> Result<(), Error> {
    plugin.set_error("TEST")?;
    anyhow::bail!("Error");
}

#[test]
fn test_extism_error() {
    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM)]);
    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world_set_error,
    );
    let mut plugin = Plugin::new(&manifest, [f], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(1024));
    assert!(output.is_err());
    assert_eq!(output.unwrap_err().root_cause().to_string(), "TEST");

    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world_set_error_bail,
    );
    let mut plugin = Plugin::new(&manifest, [f], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(1024));
    assert!(output.is_err());
    println!("{:?}", output);
    assert_eq!(output.unwrap_err().root_cause().to_string(), "TEST");
}

#[test]
fn test_extism_memdump() {
    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world_set_error,
    );
    let mut plugin = PluginBuilder::new(WASM)
        .with_wasi(true)
        .with_functions([f])
        .with_memdump("extism.mem")
        .build()
        .unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(1024));
    assert!(output.is_err());
    assert!(std::path::PathBuf::from("extism.mem").exists());
    let _ = std::fs::remove_file("extism.mem");
}

#[test]
fn test_extism_coredump() {
    let f = Function::new(
        "hello_world",
        [PTR],
        [PTR],
        UserData::default(),
        hello_world_set_error,
    );
    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM_LOOP)])
        .with_timeout(std::time::Duration::from_secs(1));
    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .with_functions([f])
        .with_coredump("extism.core")
        .build()
        .unwrap();
    let output: Result<&[u8], Error> = plugin.call("loop_forever", "abc123");
    assert!(output.is_err());
    assert!(std::path::PathBuf::from("extism.core").exists());
    let _ = std::fs::remove_file("extism.core");
}

fn hello_world_user_data(
    _plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    user_data: UserData<std::fs::File>,
) -> Result<(), Error> {
    let data = user_data.get()?;
    let mut data = data.lock().unwrap();
    let s = _plugin.memory_get_val(&inputs[0])?;
    data.write_all(s)?;
    outputs[0] = inputs[0].clone();
    Ok(())
}

#[test]
fn test_userdata() {
    let path = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("tmp");
    let output = {
        if path.exists() {
            std::fs::remove_file(&path).unwrap();
        }
        let file = std::fs::File::create(&path).unwrap();
        let f = Function::new(
            "hello_world",
            [PTR],
            [PTR],
            UserData::new(file),
            hello_world_user_data,
        );
        let mut plugin = PluginBuilder::new(WASM)
            .with_wasi(true)
            .with_functions([f])
            .build()
            .unwrap();
        let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(1024));
        assert!(output.is_ok());
        output.unwrap()
    };
    assert!(path.exists());
    assert_eq!(std::fs::read(path).unwrap(), output.as_bytes());
}

#[test]
fn test_http_not_allowed() {
    let manifest = Manifest::new([Wasm::data(WASM_HTTP)]);
    let mut plugin = PluginBuilder::new(manifest).build().unwrap();
    let res: Result<String, Error> =
        plugin.call("http_request", r#"{"url": "https://extism.org"}"#);
    assert!(res.is_err());
}

#[test]
#[cfg(feature = "http")]
fn test_http_get() {
    let manifest = Manifest::new([Wasm::data(WASM_HTTP)]).with_allowed_host("extism.org");
    let mut plugin = PluginBuilder::new(manifest).build().unwrap();
    let res: String = plugin
        .call("http_request", r#"{"url": "https://extism.org"}"#)
        .unwrap();
    assert!(!res.is_empty());
    assert!(res.contains("</html>"));
    let res1: String = plugin
        .call("http_request", r#"{"url": "https://extism.org"}"#)
        .unwrap();
    assert_eq!(res, res1);
}

#[test]
#[cfg(feature = "http")]
fn test_http_post() {
    let manifest = Manifest::new([Wasm::data(WASM_HTTP)]).with_allowed_host("httpbin.org");
    let mut plugin = PluginBuilder::new(manifest).build().unwrap();
    let res: String = plugin
        .call(
            "http_request",
            r#"{"url": "https://httpbin.org/post", "method": "POST", "data": "testing 123..."}"#,
        )
        .unwrap();
    assert!(!res.is_empty());
    assert!(res.contains(r#""data": "testing 123...""#));

    // Bigger request
    let data = "a".repeat(10000);
    let res: String = plugin
        .call(
            "http_request",
            format!(
                r#"{}"url": "https://httpbin.org/post", "method": "POST", "data": "{}"{}"#,
                "{", data, "}",
            ),
        )
        .unwrap();
    assert!(!res.is_empty());
    assert!(res.contains(&data));
}

#[test]
fn test_disable_cache() {
    // Warmup cache
    let _plugin: CountVowelsPlugin = PluginBuilder::new(WASM_NO_FUNCTIONS)
        .build()
        .unwrap()
        .try_into()
        .unwrap();

    // This should be fast
    let start = std::time::Instant::now();
    let mut plugin: CountVowelsPlugin = PluginBuilder::new(WASM_NO_FUNCTIONS)
        .build()
        .unwrap()
        .try_into()
        .unwrap();
    let t = std::time::Instant::now() - start;
    let _output: Json<Count> = plugin.count_vowels("abc123").unwrap();

    // This should take longer than the first run
    let start = std::time::Instant::now();
    let mut plugin: CountVowelsPlugin = PluginBuilder::new(WASM_NO_FUNCTIONS)
        .with_cache_disabled()
        .build()
        .unwrap()
        .try_into()
        .unwrap();
    let t1 = std::time::Instant::now() - start;
    let _output: Json<Count> = plugin.count_vowels("abc123").unwrap();

    assert!(t < t1);
}

#[test]
fn test_manifest_ptr_len() {
    let manifest = serde_json::json!({
        "wasm" : [
            {
                "data" : {
                    "ptr" : WASM_NO_FUNCTIONS.as_ptr() as u64,
                    "len" : WASM_NO_FUNCTIONS.len()
                }
            }
        ]
    });
    let mut plugin = Plugin::new(manifest.to_string().as_bytes(), [], true).unwrap();
    let output = plugin.call("count_vowels", "abc123").unwrap();
    let count: serde_json::Value = serde_json::from_slice(output).unwrap();
    assert_eq!(count.get("count").unwrap().as_i64().unwrap(), 1);
}
