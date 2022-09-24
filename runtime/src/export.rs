/// All the functions in the file are exposed from inside WASM plugins
use crate::*;

/// Get the input length
/// Params: none
/// Returns: i64 (length)
pub(crate) fn input_length(
    caller: Caller<Internal>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    output[0] = Val::I64(data.input_length as i64);
    Ok(())
}

/// Load a byte from input
/// Params: i64 (offset)
/// Returns: i32 (byte)
pub(crate) fn input_load_u8(
    caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    if data.input.is_null() {
        return Ok(());
    }
    output[0] = unsafe { Val::I32(*data.input.add(input[0].unwrap_i64() as usize) as i32) };
    Ok(())
}

/// Load an unsigned 64 bit integer from input
/// Params: i64 (offset)
/// Returns: i64 (int)
pub(crate) fn input_load_u64(
    caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    if data.input.is_null() {
        return Ok(());
    }
    let offs = input[0].unwrap_i64() as usize;
    let slice = unsafe { std::slice::from_raw_parts(data.input.add(offs), 8) };
    let byte = u64::from_ne_bytes(slice.try_into().unwrap());
    output[0] = Val::I64(byte as i64);
    Ok(())
}

/// Store a byte in memory
/// Params: i64 (offset), i32 (byte)
/// Returns: none
pub(crate) fn store_u8(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let byte = input[1].unwrap_i32() as u8;
    data.memory_mut()
        .store_u8(input[0].unwrap_i64() as usize, byte)
        .map_err(|_| Trap::new("Write error"))?;
    Ok(())
}

/// Load a byte from memory
/// Params: i64 (offset)
/// Returns: i32 (byte)
pub(crate) fn load_u8(
    caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    let byte = data
        .memory()
        .load_u8(input[0].unwrap_i64() as usize)
        .map_err(|_| Trap::new("Read error"))?;
    output[0] = Val::I32(byte as i32);
    Ok(())
}

/// Store an unsigned 32 bit integer in memory
/// Params: i64 (offset), i32 (int)
/// Returns: none
pub(crate) fn store_u32(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let b = input[1].unwrap_i32() as u32;
    data.memory_mut()
        .store_u32(input[0].unwrap_i64() as usize, b)
        .map_err(|_| Trap::new("Write error"))?;
    Ok(())
}

/// Load an unsigned 32 bit integer from memory
/// Params: i64 (offset)
/// Returns: i32 (int)
pub(crate) fn load_u32(
    caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    let b = data
        .memory()
        .load_u32(input[0].unwrap_i64() as usize)
        .map_err(|_| Trap::new("Read error"))?;
    output[0] = Val::I32(b as i32);
    Ok(())
}

/// Store an unsigned 64 bit integer in memory
/// Params: i64 (offset), i64 (int)
/// Returns: none
pub(crate) fn store_u64(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let b = input[1].unwrap_i64() as u64;
    data.memory_mut()
        .store_u64(input[0].unwrap_i64() as usize, b)
        .map_err(|_| Trap::new("Write error"))?;
    Ok(())
}

/// Load an unsigned 64 bit integer from memory
/// Params: i64 (offset)
/// Returns: i64 (int)
pub(crate) fn load_u64(
    caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    let byte = data
        .memory()
        .load_u64(input[0].unwrap_i64() as usize)
        .map_err(|_| Trap::new("Read error"))?;
    output[0] = Val::I64(byte as i64);
    Ok(())
}

/// Set output offset and length
/// Params: i64 (offset), i64 (length)
/// Returns: none
pub(crate) fn output_set(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    data.output_offset = input[0].unwrap_i64() as usize;
    data.output_length = input[1].unwrap_i64() as usize;
    Ok(())
}

/// Allocate bytes
/// Params: i64 (length)
/// Returns: i64 (offset)
pub(crate) fn alloc(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offs = data.memory_mut().alloc(input[0].unwrap_i64() as _)?;
    output[0] = Val::I64(offs.offset as i64);

    Ok(())
}

/// Free memory
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn free(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offset = input[0].unwrap_i64() as usize;
    data.memory_mut().free(offset);
    Ok(())
}

/// Set the error message, this can be checked by the host program
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn error_set(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offset = input[0].unwrap_i64() as usize;

    if offset == 0 {
        data.plugin_mut().clear_error();
        return Ok(());
    }

    let plugin = data.plugin_mut();
    let s = plugin.memory.get_str(offset)?;
    plugin.set_error(s);
    Ok(())
}

/// Get a configuration value
/// Params: i64 (offset)
/// Returns: i64 (offset)
pub(crate) fn config_get(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let plugin = data.plugin_mut();

    let key = plugin.memory.get_str(input[0].unwrap_i64() as usize)?;
    let val = plugin.manifest.as_ref().config.get(key);
    let mem = match val {
        Some(f) => plugin.memory.alloc_bytes(f)?,
        None => {
            output[0] = Val::I64(0);
            return Ok(());
        }
    };
    output[0] = Val::I64(mem.offset as i64);
    Ok(())
}

/// Get a variable
/// Params: i64 (offset)
/// Returns: i64 (offset)
pub(crate) fn var_get(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let plugin = data.plugin_mut();

    let offset = input[0].unwrap_i64() as usize;
    let key = plugin.memory.get_str(offset)?;
    let val = plugin.vars.get(key);

    let mem = match val {
        Some(f) => plugin.memory.alloc_bytes(f)?,
        None => {
            output[0] = Val::I64(0);
            return Ok(());
        }
    };

    output[0] = Val::I64(mem.offset as i64);
    Ok(())
}

/// Set a variable, if the value offset is 0 then the provided key will be removed
/// Params: i64 (key offset), i64 (value offset)
/// Returns: none
pub(crate) fn var_set(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let plugin = data.plugin_mut();

    let mut size = 0;
    for v in plugin.vars.values() {
        size += v.len();
    }

    let voffset = input[1].unwrap_i64() as usize;

    // If the store is larger than 100MB then stop adding things
    if size > 1024 * 1024 * 100 && voffset != 0 {
        return Err(Trap::new("Variable store is full"));
    }

    let key = plugin.memory.get_str(input[0].unwrap_i64() as usize)?;

    // Remove if the value offset is 0
    if voffset == 0 {
        plugin.vars.remove(key);
        return Ok(());
    }

    let value = plugin.memory.get(voffset)?;

    // Insert the value from memory into the `vars` map
    plugin.vars.insert(key.to_string(), value.to_vec());

    Ok(())
}

/// Make an HTTP request
/// Params: i64 (offset to JSON encoded HttpRequest), i64 (offset to body or 0)
/// Returns: i64 (offset)
pub(crate) fn http_request(
    #[allow(unused_mut)] mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    #[cfg(not(feature = "http"))]
    {
        let _ = (caller, input);

        output[0] = Val::I64(0 as i64);
        error!("http_request is not enabled");
        return Ok(());
    }

    #[cfg(feature = "http")]
    {
        use std::io::Read;
        let data: &mut Internal = caller.data_mut();
        let http_req_offset = input[0].unwrap_i64() as usize;

        let req: extism_manifest::HttpRequest =
            serde_json::from_slice(data.memory().get(http_req_offset)?)
                .map_err(|_| Trap::new("Invalid http request"))?;

        let body_offset = input[1].unwrap_i64() as usize;

        let mut r = ureq::request(req.method.as_deref().unwrap_or("GET"), &req.url);

        for (k, v) in req.header.iter() {
            r = r.set(k, v);
        }

        let mut res = if body_offset > 0 {
            let buf = data.memory().get(body_offset)?;
            r.send_bytes(buf)
                .map_err(|e| Trap::new(&format!("Request error: {e:?}")))?
                .into_reader()
        } else {
            r.call()
                .map_err(|e| Trap::new(format!("{:?}", e)))?
                .into_reader()
        };

        let mut buf = Vec::new();
        res.read_to_end(&mut buf)
            .map_err(|e| Trap::new(format!("{:?}", e)))?;

        let mem = data.memory_mut().alloc_bytes(buf)?;

        output[0] = Val::I64(mem.offset as i64);
        Ok(())
    }
}

/// Get the length of an allocated block given the offset
/// Params: i64 (offset)
/// Returns: i64 (length or 0)
pub(crate) fn length(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offset = input[0].unwrap_i64() as usize;
    if offset == 0 {
        output[0] = Val::I64(0);
        return Ok(());
    }
    let length = match data.memory().block_length(offset) {
        Some(x) => x,
        None => return Err(Trap::new("Unable to find length for offset")),
    };
    output[0] = Val::I64(length as i64);
    Ok(())
}

pub fn log(
    level: log::Level,
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    let offset = input[0].unwrap_i64() as usize;
    let buf = data.memory().get(offset)?;

    match std::str::from_utf8(buf) {
        Ok(buf) => log::log!(level, "{}", buf),
        Err(_) => log::log!(level, "{:?}", buf),
    }
    Ok(())
}

/// Write to logs (warning)
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn log_warn(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Warn, caller, input, _output)
}

/// Write to logs (info)
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn log_info(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Info, caller, input, _output)
}

/// Write to logs (debug)
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn log_debug(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Debug, caller, input, _output)
}

/// Write to logs (error)
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn log_error(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Error, caller, input, _output)
}
