pub mod bindings;

use bindings::*;

pub struct Host {
    input: Vec<u8>,
}

impl Default for Host {
    fn default() -> Self {
        Host::new()
    }
}

pub struct Vars<'a>(&'a Host);

pub struct Memory {
    pub offset: u64,
    pub length: u64,
}

impl Memory {
    pub fn load(&self, mut buf: impl AsMut<[u8]>) {
        let buf = buf.as_mut();
        unsafe { extism_load(self.offset, &mut buf[0..self.length as usize]) }
    }

    pub fn store(&mut self, buf: impl AsRef<[u8]>) {
        let buf = buf.as_ref();
        unsafe { extism_store(self.offset, &buf[0..self.length as usize]) }
    }
}

impl Drop for Memory {
    fn drop(&mut self) {
        unsafe { extism_free(self.offset) }
    }
}

impl<'a> Vars<'a> {
    pub fn new(host: &'a Host) -> Self {
        Vars(host)
    }

    pub fn get(&self, key: impl AsRef<str>) -> Option<Vec<u8>> {
        let mem = self.0.alloc_bytes(key.as_ref().as_bytes());

        let offset = unsafe { extism_kv_get(mem.offset) };
        let len = unsafe { extism_length(offset) };

        if offset == 0 || len == 0 {
            return None;
        }

        let mut buf = vec![0; len as usize];
        unsafe {
            extism_load(offset, &mut buf);
        }
        Some(buf)
    }

    pub fn set(&mut self, key: impl AsRef<str>, val: impl AsRef<[u8]>) {
        let key = self.0.alloc_bytes(key.as_ref().as_bytes());
        let val = self.0.alloc_bytes(val.as_ref());
        unsafe { extism_kv_set(key.offset, val.offset) }
    }

    pub fn remove(&mut self, key: impl AsRef<str>) {
        let key = self.0.alloc_bytes(key.as_ref().as_bytes());
        unsafe { extism_kv_set(key.offset, 0) }
    }
}

impl Host {
    pub fn new() -> Host {
        unsafe {
            let input_offset = extism_input_offset();
            let input_length = extism_length(input_offset);
            let mut input = vec![0; input_length as usize];
            extism_load(input_offset, &mut input);
            Host { input }
        }
    }

    pub fn alloc(&self, length: usize) -> Memory {
        let length = length as u64;
        let offset = unsafe { extism_alloc(length) };
        Memory { offset, length }
    }

    pub fn alloc_bytes(&self, data: impl AsRef<[u8]>) -> Memory {
        let data = data.as_ref();
        let length = data.len() as u64;
        let offset = unsafe { extism_alloc(length) };
        Memory { offset, length }
    }

    pub fn input(&self) -> &[u8] {
        self.input.as_slice()
    }

    pub fn input_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.input.as_slice()) }
    }

    pub fn output(&self, data: impl AsRef<[u8]>) {
        let len = data.as_ref().len();
        unsafe {
            let offs = extism_alloc(len as u64);
            extism_store(offs, data.as_ref());
            extism_output_set(offs, len as u64);
        }
    }

    pub fn config(&self, key: impl AsRef<str>) -> String {
        let mem = self.alloc_bytes(key.as_ref().as_bytes());

        let offset = unsafe { extism_config_get(mem.offset) };
        let len = unsafe { extism_length(offset) };

        if offset == 0 || len == 0 {
            return String::new();
        }

        let mut buf = vec![0; len as usize];
        unsafe {
            extism_load(offset, &mut buf);
            String::from_utf8_unchecked(buf)
        }
    }

    pub fn vars(&self) -> Vars {
        Vars::new(self)
    }
}
