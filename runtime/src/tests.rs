use super::*;
use std::time::Instant;

const WASM: &[u8] = include_bytes!("../../wasm/code-functions.wasm");
const WASM_LOOP: &[u8] = include_bytes!("../../wasm/loop.wasm");
const WASM_GLOBALS: &[u8] = include_bytes!("../../wasm/globals.wasm");

fn hello_world(
    plugin: &mut CurrentPlugin,
    inputs: &[Val],
    outputs: &mut [Val],
    _user_data: UserData,
) -> Result<(), Error> {
    let handle = plugin.memory_from_val(&inputs[0]).unwrap();
    let input = plugin.memory_str(handle).unwrap().to_string();

    let output = plugin.alloc(&input).unwrap();
    outputs[0] = plugin.memory_to_val(output);
    Ok(())
}

fn hello_world_panic(
    _plugin: &mut CurrentPlugin,
    _inputs: &[Val],
    _outputs: &mut [Val],
    _user_data: UserData,
) -> Result<(), Error> {
    panic!("This should not run");
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Debug)]
pub struct Count {
    count: usize,
}

#[test]
fn it_works() {
    let wasm_start = Instant::now();
    assert!(set_log_file("test.log", log::Level::Trace).is_ok());
    let f = Function::new(
        "hello_world",
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world,
    )
    .with_namespace("env");
    let g = Function::new(
        "hello_world",
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world_panic,
    )
    .with_namespace("test");

    let mut plugin = Plugin::new(WASM, [f, g], true).unwrap();
    println!("register loaded plugin: {:?}", wasm_start.elapsed());

    let repeat = 1182;
    let input = "aeiouAEIOU____________________________________&smtms_y?".repeat(repeat);
    let Json(count): Json<Count> = plugin.call("count_vowels", &input).unwrap();

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
        for i in 0..input.len() {
            if input[i] == b'A'
                || input[i] == b'E'
                || input[i] == b'I'
                || input[i] == b'O'
                || input[i] == b'U'
                || input[i] == b'a'
                || input[i] == b'e'
                || input[i] == b'i'
                || input[i] == b'o'
                || input[i] == b'u'
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
}

#[test]
fn test_plugin_threads() {
    let p = std::sync::Arc::new(std::sync::Mutex::new(
        PluginBuilder::new_with_module(WASM)
            .with_function(
                "hello_world",
                [ValType::I64],
                [ValType::I64],
                None,
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
                let Json(count): Json<Count> =
                    plugin.call("count_vowels", "this is a test aaa").unwrap();
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
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world,
    );

    let mut plugin = Plugin::new(WASM_LOOP, [f], true).unwrap();
    let handle = plugin.cancel_handle();

    let start = std::time::Instant::now();
    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_secs(1));
        assert!(handle.cancel().is_ok());
    });
    let _output: Result<&[u8], Error> = plugin.call("infinite_loop", "abc123");
    let end = std::time::Instant::now();
    let time = end - start;
    println!("Cancelled plugin ran for {:?}", time);
    // std::io::stdout().write_all(output).unwrap();
}

#[test]
fn test_timeout() {
    let f = Function::new(
        "hello_world",
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world,
    );

    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM_LOOP)])
        .with_timeout(std::time::Duration::from_secs(1));
    let mut plugin = Plugin::new_with_manifest(&manifest, [f], true).unwrap();

    let start = std::time::Instant::now();
    let _output: Result<&[u8], Error> = plugin.call("infinite_loop", "abc123");
    let end = std::time::Instant::now();
    let time = end - start;
    println!("Timed out plugin ran for {:?}", time);
    // std::io::stdout().write_all(output).unwrap();
}

#[test]
fn test_multiple_instantiations() {
    let f = Function::new(
        "hello_world",
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world,
    );

    let mut plugin = Plugin::new(WASM, [f], true).unwrap();

    // This is 10,001 because the wasmtime store limit is 10,000 - we want to test
    // that our reinstantiation process is working and that limit is never hit.
    for _ in 0..10001 {
        let _output: &[u8] = plugin.call("count_vowels", "abc123").unwrap();
    }
}

#[test]
fn test_globals() {
    let mut plugin = Plugin::new(WASM_GLOBALS, [], true).unwrap();
    for i in 0..1000 {
        let Json(count): Json<Count> = plugin.call("globals", "").unwrap();
        assert_eq!(count.count, i);
    }
}

#[test]
fn test_toml_manifest() {
    let f = Function::new(
        "hello_world",
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world,
    );

    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM)])
        .with_timeout(std::time::Duration::from_secs(1));

    let manifest_toml = toml::to_string_pretty(&manifest).unwrap();
    let mut plugin = Plugin::new(manifest_toml.as_bytes(), [f], true).unwrap();

    let output = plugin.call("count_vowels", "abc123").unwrap();
    let count: serde_json::Value = serde_json::from_slice(output).unwrap();
    assert_eq!(count.get("count").unwrap().as_i64().unwrap(), 1);
}
