pub use extism_manifest::{self as manifest, Manifest};
pub use extism_runtime::{
    sdk as bindings, Function, MemoryBlock, Plugin as CurrentPlugin, UserData, Val, ValType,
};

mod context;
mod plugin;
mod plugin_builder;

pub use context::Context;
pub use plugin::{CancelHandle, Plugin};
pub use plugin_builder::PluginBuilder;
pub type Error = anyhow::Error;

/// Gets the version of Extism
pub fn extism_version() -> String {
    let err = unsafe { bindings::extism_version() };
    let buf = unsafe { std::ffi::CStr::from_ptr(err) };
    return buf.to_str().unwrap().to_string();
}

/// Set the log file and level, this is a global setting
pub fn set_log_file(filename: impl AsRef<std::path::Path>, log_level: Option<log::Level>) -> bool {
    let log_level = log_level.map(|x| x.as_str());
    unsafe {
        return bindings::extism_log_file(
            filename.as_ref().as_os_str().to_string_lossy().as_ptr() as *const _,
            log_level.map(|x| x.as_ptr()).unwrap_or(std::ptr::null()) as *const _,
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    const WASM: &[u8] = include_bytes!("../../wasm/code-functions.wasm");
    const WASM_LOOP: &[u8] = include_bytes!("../../wasm/loop.wasm");

    fn hello_world(
        _plugin: &mut CurrentPlugin,
        inputs: &[Val],
        outputs: &mut [Val],
        _user_data: UserData,
    ) -> Result<(), Error> {
        outputs[0] = inputs[0].clone();
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

    #[test]
    fn it_works() {
        let wasm_start = Instant::now();
        set_log_file("test.log", Some(log::Level::Info));
        let context = Context::new();
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

        let functions = [&f, &g];
        let mut plugin = Plugin::new(&context, WASM, functions, true).unwrap();
        println!("register loaded plugin: {:?}", wasm_start.elapsed());

        let repeat = 1182;
        let input = "aeiouAEIOU____________________________________&smtms_y?".repeat(repeat);
        let data = plugin.call("count_vowels", &input).unwrap();

        assert_eq!(
            data,
            b"{\"count\": 11820}",
            "expecting vowel count of {}, input size: {}, output size: {}",
            10 * repeat,
            input.len(),
            data.len()
        );

        println!(
            "register plugin + function call: {:?}, sent input size: {} bytes",
            wasm_start.elapsed(),
            input.len()
        );

        println!("--------------");

        let test_times = (0..100)
            .map(|_| {
                let test_start = Instant::now();
                plugin.call("count_vowels", &input).unwrap();
                test_start.elapsed()
            })
            .collect::<Vec<_>>();

        let native_test = || {
            let native_start = Instant::now();
            // let native_vowel_count = input
            //     .chars()
            //     .filter(|c| match c {
            //         'A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o' | 'u' => true,
            //         _ => false,
            //     })
            //     .collect::<Vec<_>>()
            //     .len();

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
    fn test_threads() {
        use std::io::Write;
        std::thread::spawn(|| {
            let context = Context::new();
            let f = Function::new(
                "hello_world",
                [ValType::I64],
                [ValType::I64],
                None,
                hello_world,
            );
            let mut plugin = Plugin::new(&context, WASM, [&f], true).unwrap();
            let output = plugin.call("count_vowels", "this is a test").unwrap();
            std::io::stdout().write_all(output).unwrap();
        });

        let f = Function::new(
            "hello_world",
            [ValType::I64],
            [ValType::I64],
            None,
            hello_world,
        );

        let g = f.clone();
        std::thread::spawn(move || {
            let context = Context::new();
            let mut plugin = PluginBuilder::new_with_module(WASM)
                .with_function(&g)
                .with_wasi(true)
                .build(&context)
                .unwrap();
            let output = plugin.call("count_vowels", "this is a test aaa").unwrap();
            std::io::stdout().write_all(output).unwrap();
        });

        let context = Context::new();
        let mut plugin = Plugin::new(&context, WASM, [&f], true).unwrap();
        let output = plugin.call("count_vowels", "abc123").unwrap();
        std::io::stdout().write_all(output).unwrap();
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

        let context = Context::new();
        let mut plugin = Plugin::new(&context, WASM_LOOP, [&f], true).unwrap();
        let handle = plugin.cancel_handle();

        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_secs(1));
            handle.cancel();
        });

        let start = std::time::Instant::now();
        let _output = plugin.call("infinite_loop", "abc123");
        let end = std::time::Instant::now();
        let time = end - start;
        println!("Cancelled plugin ran for {:?}", time);
        // std::io::stdout().write_all(output).unwrap();
    }
}
