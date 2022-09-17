use std::collections::BTreeMap;

use extism_manifest::Manifest;

#[allow(non_camel_case_types)]
mod bindings;

#[repr(transparent)]
pub struct Plugin(isize);

#[derive(Debug)]
pub enum Error {
    UnableToLoadPlugin(String),
    Message(String),
    Json(serde_json::Error),
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Json(e)
    }
}

pub fn reset() {
    unsafe {
        bindings::extism_reset();
    }
}

impl Plugin {
    pub fn new_with_manifest(manifest: &Manifest, wasi: bool) -> Result<Plugin, Error> {
        let data = serde_json::to_vec(manifest)?;
        Self::new(data, wasi)
    }

    pub fn new(data: impl AsRef<[u8]>, wasi: bool) -> Result<Plugin, Error> {
        let plugin = unsafe {
            bindings::extism_plugin_register(
                data.as_ref().as_ptr(),
                data.as_ref().len() as u64,
                wasi,
            )
        };

        if plugin < 0 {
            let err = unsafe { bindings::extism_error(-1) };
            let buf = unsafe { std::ffi::CStr::from_ptr(err) };
            let buf = buf.to_str().unwrap().to_string();
            return Err(Error::UnableToLoadPlugin(buf));
        }

        Ok(Plugin(plugin as isize))
    }

    pub fn update(&mut self, data: impl AsRef<[u8]>, wasi: bool) -> Result<bool, Error> {
        let b = unsafe {
            bindings::extism_plugin_update(
                self.0 as i32,
                data.as_ref().as_ptr(),
                data.as_ref().len() as u64,
                wasi,
            )
        };
        if b {
            return Ok(true);
        }

        let err = unsafe { bindings::extism_error(-1) };
        if !err.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(err) };
            return Err(Error::Message(s.to_str().unwrap().to_string()));
        }

        return Err(Error::Message("extism_plugin_update failed".to_string()));
    }

    pub fn update_manifest(&mut self, manifest: &Manifest, wasi: bool) -> Result<bool, Error> {
        let data = serde_json::to_vec(manifest)?;
        self.update(data, wasi)
    }

    pub fn set_config(&self, config: &BTreeMap<String, String>) -> Result<(), Error> {
        let encoded = serde_json::to_vec(config)?;
        unsafe {
            bindings::extism_plugin_config(
                self.0 as i32,
                encoded.as_ptr() as *const _,
                encoded.len() as u64,
            )
        };
        Ok(())
    }

    pub fn with_config(self, config: &BTreeMap<String, String>) -> Result<Self, Error> {
        self.set_config(config)?;
        Ok(self)
    }

    pub fn set_log_file(
        &self,
        filename: impl AsRef<std::path::Path>,
        log_level: Option<log::LevelFilter>,
    ) {
        let log_level = log_level.map(|x| x.as_str());
        unsafe {
            bindings::extism_log_file(
                filename.as_ref().as_os_str().to_string_lossy().as_ptr() as *const _,
                log_level.map(|x| x.as_ptr()).unwrap_or(std::ptr::null()) as *const _,
            );
        }
    }

    pub fn with_log_file(
        self,
        filename: impl AsRef<std::path::Path>,
        log_level: Option<log::LevelFilter>,
    ) -> Self {
        self.set_log_file(filename, log_level);
        self
    }

    pub fn has_function(&self, name: impl AsRef<str>) -> bool {
        let name = std::ffi::CString::new(name.as_ref()).expect("Invalid function name");
        unsafe { bindings::extism_function_exists(self.0 as i32, name.as_ptr() as *const _) }
    }

    pub fn call(&self, name: impl AsRef<str>, input: impl AsRef<[u8]>) -> Result<&[u8], Error> {
        let name = std::ffi::CString::new(name.as_ref()).expect("Invalid function name");
        let rc = unsafe {
            bindings::extism_call(
                self.0 as i32,
                name.as_ptr() as *const _,
                input.as_ref().as_ptr() as *const _,
                input.as_ref().len() as u64,
            )
        };

        if rc != 0 {
            let err = unsafe { bindings::extism_error(self.0 as i32) };
            if !err.is_null() {
                let s = unsafe { std::ffi::CStr::from_ptr(err) };
                return Err(Error::Message(s.to_str().unwrap().to_string()));
            }

            return Err(Error::Message("extism_call failed".to_string()));
        }

        let out_len = unsafe { bindings::extism_output_length(self.0 as i32) };
        unsafe {
            let ptr = bindings::extism_output_get(self.0 as i32);
            Ok(std::slice::from_raw_parts(ptr, out_len as usize))
        }
    }
}

impl Drop for Plugin {
    fn drop(&mut self) {
        unsafe { bindings::extism_plugin_destroy(self.0 as i32) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn it_works() {
        let wasm = include_bytes!("../../wasm/code.wasm");
        let wasm_start = Instant::now();
        let plugin = Plugin::new(wasm, false)
            .unwrap()
            .with_log_file("test.log", Some(log::LevelFilter::Info));
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
}
