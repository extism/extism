use crate::*;
use std::time::Instant;

const WASM: &[u8] = include_bytes!("../../../wasm/code-functions.wasm");
const WASM_NO_FUNCTIONS: &[u8] = include_bytes!("../../../wasm/code.wasm");
const WASM_LOOP: &[u8] = include_bytes!("../../../wasm/loop.wasm");
const WASM_GLOBALS: &[u8] = include_bytes!("../../../wasm/globals.wasm");
const WASM_REFLECT: &[u8] = include_bytes!("../../../wasm/reflect.wasm");
const KERNEL: &[u8] = include_bytes!("../extism-runtime.wasm");

host_fn!(hello_world (a: String) -> String { a });

// Which is the same as:
// fn hello_world(
//     plugin: &mut CurrentPlugin,
//     inputs: &[Val],
//     outputs: &mut [Val],
//     _user_data: UserData,
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
    let output: Result<&[u8], Error> = plugin.call("infinite_loop", "abc123");
    let end = std::time::Instant::now();
    let time = end - start;
    println!("Timed out plugin ran for {:?}", time);
    assert!(output.unwrap_err().root_cause().to_string() == "timeout");
    // std::io::stdout().write_all(output).unwrap();
}

typed_plugin!(TestTypedPluginGenerics {
    count_vowels<T: FromBytes<'a>>(&str) -> T
});

typed_plugin!(CountVowelsPlugin {
    count_vowels(&str) -> Json<Count>;
});

#[test]
fn test_typed_plugin_macro() {
    let f = Function::new(
        "hello_world",
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world,
    );

    let mut plugin: CountVowelsPlugin = Plugin::new(WASM, [f], true).unwrap().into();

    let Json(output0): Json<Count> = plugin.count_vowels("abc123").unwrap();
    let Json(output1): Json<Count> = plugin.0.call("count_vowels", "abc123").unwrap();

    assert_eq!(output0, output1)
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

    let mut plugin: CountVowelsPlugin = Plugin::new(WASM, [f], true).unwrap().into();

    // This is 10,001 because the wasmtime store limit is 10,000 - we want to test
    // that our reinstantiation process is working and that limit is never hit.
    for _ in 0..10001 {
        let _output: Json<Count> = plugin.count_vowels("abc123").unwrap();
    }
}

#[test]
fn test_globals() {
    let mut plugin = Plugin::new(WASM_GLOBALS, [], true).unwrap();
    for i in 0..1000 {
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
        [ValType::I64],
        [ValType::I64],
        None,
        hello_world,
    );

    let mut plugin = Plugin::new(WASM_REFLECT, [f], true).unwrap();

    for i in 1..65540 {
        let input = "a".repeat(i);
        let output = plugin.call("reflect", input.clone());
        let output = std::str::from_utf8(output.unwrap()).unwrap();
        assert_eq!(output, input);
    }
}

#[test]
fn test_memory_max() {
    // Should fail with memory.max set
    let manifest =
        Manifest::new([extism_manifest::Wasm::data(WASM_NO_FUNCTIONS)]).with_memory_max(16);
    let mut plugin = Plugin::new_with_manifest(&manifest, [], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(65536 * 2));
    assert!(output.is_err());
    assert!(output.unwrap_err().root_cause().to_string() == "oom");

    // Should pass with memory.max set to a large enough number
    let manifest =
        Manifest::new([extism_manifest::Wasm::data(WASM_NO_FUNCTIONS)]).with_memory_max(17);
    let mut plugin = Plugin::new_with_manifest(&manifest, [], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(65536 * 2));
    assert!(output.is_ok());

    // Should pass without it
    let manifest = Manifest::new([extism_manifest::Wasm::data(WASM_NO_FUNCTIONS)]);
    let mut plugin = Plugin::new_with_manifest(&manifest, [], true).unwrap();
    let output: Result<String, Error> = plugin.call("count_vowels", "a".repeat(65536 * 2));
    assert!(output.is_ok());
}

#[test]
fn test_kernel() {
    let config = wasmtime::Config::new();
    let engine = wasmtime::Engine::new(&config).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let module = wasmtime::Module::new(&engine, KERNEL).unwrap();
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();

    let extism_alloc = |mut store: &mut wasmtime::Store<_>, n: u64| {
        let out_alloc = &mut [Val::I64(0)];
        instance
            .get_func(&mut store, "extism_alloc")
            .unwrap()
            .call(&mut store, &[Val::I64(n as i64)], out_alloc)
            .unwrap();
        out_alloc[0].unwrap_i64() as u64
    };

    let extism_length = |mut store: &mut wasmtime::Store<_>, p: u64| {
        let out = &mut [Val::I64(0)];
        instance
            .get_func(&mut store, "extism_length")
            .unwrap()
            .call(&mut store, &[Val::I64(p as i64)], out)
            .unwrap();
        out[0].unwrap_i64() as u64
    };

    assert_eq!(extism_alloc(&mut store, 0), 0);

    let p = extism_alloc(&mut store, 64);
    assert!(p > 0);
    assert_eq!(extism_length(&mut store, p), 64);
}
