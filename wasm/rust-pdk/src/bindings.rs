extern "C" {
    pub fn extism_input_offset() -> u64;
    pub fn extism_length(offs: u64) -> u64;
    pub fn extism_alloc(length: u64) -> u64;
    pub fn extism_free(offs: u64);
    pub fn extism_output_set(offs: u64, length: u64);
    pub fn extism_error_set(offs: u64);
    pub fn extism_store_u8(offs: u64, data: u8);
    pub fn extism_load_u8(offs: u64) -> u8;
    pub fn extism_store_u32(offs: u64, data: u32);
    pub fn extism_load_u32(offs: u64) -> u32;
    pub fn extism_store_u64(offs: u64, data: u64);
    pub fn extism_load_u64(offs: u64) -> u64;
    pub fn extism_file_read(fd: i32) -> u64;
    pub fn extism_file_write(fd: i32, offs: u64);
    pub fn extism_config_get(offs: u64) -> u64;
    pub fn extism_kv_get(offs: u64) -> u64;
    pub fn extism_kv_set(offs: u64, offs1: u64);
}

/// # Safety
///
/// This function is used to access WASM memory
pub unsafe fn extism_load(offs: u64, data: &mut [u8]) {
    let ptr = data.as_mut_ptr();

    let mut index = 0;
    let mut left;
    let len = data.len();
    while index < len {
        left = len - index;
        if left < 8 {
            data[index] = extism_load_u8(offs + index as u64);
            index += 1;
            continue;
        }

        let x = extism_load_u64(offs + index as u64);
        (ptr as *mut u64).add(index / 8).write(x);
        index += 8;
    }
}

/// # Safety
///
/// This function is used to access WASM memory
pub unsafe fn extism_store(offs: u64, data: &[u8]) {
    let ptr = data.as_ptr();

    let mut index = 0;
    let mut left;
    let len = data.len();
    while index < len {
        left = len - index;
        if left < 8 {
            extism_store_u8(offs + index as u64, data[index]);
            index += 1;
            continue;
        }

        extism_store_u64(
            offs + index as u64,
            (ptr as *const u64).add(index / 8).read(),
        );
        index += 8;
    }
}
