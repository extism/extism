/// All the functions in the file are exposed from inside WASM plugins
use crate::*;

// This macro unwraps input arguments to prevent functions from panicking,
// it should be used instead of `Val::unwrap_*` functions
#[macro_export]
macro_rules! args {
    ($input:expr, $index:expr, $ty:ident) => {
        match $input[$index].$ty() {
            Some(x) => x,
            None => return Err($crate::Error::msg("Invalid input type"))
        }
    };
    ($input:expr, $(($index:expr, $ty:ident)),*$(,)?) => {
        ($(
            $crate::args!($input, $index, $ty),
        )*)
    };
}

/// Get the input length
/// Params: none
/// Returns: i64 (length)
pub(crate) fn input_length(
    caller: Caller<Internal>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
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
) -> Result<(), Error> {
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
) -> Result<(), Error> {
    let data: &Internal = caller.data();
    if data.input.is_null() {
        return Ok(());
    }
    let offs = args!(input, 0, i64) as usize;
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
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let (offset, byte) = args!(input, (0, i64), (1, i32));
    data.memory_mut().store_u8(offset as usize, byte as u8)?;
    Ok(())
}

/// Load a byte from memory
/// Params: i64 (offset)
/// Returns: i32 (byte)
pub(crate) fn load_u8(
    caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &Internal = caller.data();
    let offset = args!(input, 0, i64) as usize;
    let byte = data.memory().load_u8(offset)?;
    output[0] = Val::I32(byte as i32);
    Ok(())
}

/// Store an unsigned 64 bit integer in memory
/// Params: i64 (offset), i64 (int)
/// Returns: none
pub(crate) fn store_u64(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let (offset, b) = args!(input, (0, i64), (1, i64));
    data.memory_mut().store_u64(offset as usize, b as u64)?;
    Ok(())
}

/// Load an unsigned 64 bit integer from memory
/// Params: i64 (offset)
/// Returns: i64 (int)
pub(crate) fn load_u64(
    caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &Internal = caller.data();
    let offset = args!(input, 0, i64) as usize;
    let byte = data.memory().load_u64(offset)?;
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
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let (offset, length) = args!(input, (0, i64), (1, i64));
    data.output_offset = offset as usize;
    data.output_length = length as usize;
    Ok(())
}

/// Allocate bytes
/// Params: i64 (length)
/// Returns: i64 (offset)
pub(crate) fn alloc(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
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
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let offset = args!(input, 0, i64) as usize;
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
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let offset = args!(input, 0, i64) as usize;

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
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let plugin = data.plugin_mut();

    let offset = args!(input, 0, i64) as usize;
    let key = plugin.memory.get_str(offset)?;
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
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let plugin = data.plugin_mut();

    let offset = args!(input, 0, i64) as usize;
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
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let plugin = data.plugin_mut();

    let mut size = 0;
    for v in plugin.vars.values() {
        size += v.len();
    }

    let voffset = args!(input, 1, i64) as usize;

    // If the store is larger than 100MB then stop adding things
    if size > 1024 * 1024 * 100 && voffset != 0 {
        return Err(Error::msg("Variable store is full"));
    }

    let key_offs = args!(input, 0, i64) as usize;
    let key = plugin.memory.get_str(key_offs)?;

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
) -> Result<(), Error> {
    #[cfg(not(feature = "http"))]
    {
        let _ = (caller, input);

        output[0] = Val::I64(0);
        error!("http_request is not enabled");
        return Ok(());
    }

    #[cfg(feature = "http")]
    {
        use std::io::Read;
        let data: &mut Internal = caller.data_mut();
        let http_req_offset = args!(input, 0, i64) as usize;

        let req: extism_manifest::HttpRequest =
            serde_json::from_slice(data.memory().get(http_req_offset)?)?;

        let body_offset = args!(input, 1, i64) as usize;

        let url = match url::Url::parse(&req.url) {
            Ok(u) => u,
            Err(e) => return Err(Error::msg(format!("Invalid URL: {e:?}"))),
        };
        let allowed_hosts = &data.plugin().manifest.as_ref().allowed_hosts;
        let host_str = url.host_str().unwrap_or_default();
        if let Some(allowed_hosts) = allowed_hosts {
            let host_matches_allowed = allowed_hosts.iter().any(|url| {
                let pat = match glob::Pattern::new(url) {
                    Ok(x) => x,
                    Err(_) => return url == host_str,
                };

                pat.matches(host_str)
            });
            if !host_matches_allowed {
                return Err(Error::msg(format!(
                    "HTTP request to {} is not allowed",
                    req.url
                )));
            }
        }

        let mut r = ureq::request(req.method.as_deref().unwrap_or("GET"), &req.url);

        for (k, v) in req.headers.iter() {
            r = r.set(k, v);
        }

        let res = if body_offset > 0 {
            let buf = data.memory().get(body_offset)?;
            let res = r.send_bytes(buf)?;
            data.http_status = res.status();
            res.into_reader()
        } else {
            let res = r.call()?;
            data.http_status = res.status();
            res.into_reader()
        };

        let mut buf = Vec::new();
        res.take(1024 * 1024 * 50) // TODO: make this limit configurable
            .read_to_end(&mut buf)?;

        let mem = data.memory_mut().alloc_bytes(buf)?;

        output[0] = Val::I64(mem.offset as i64);
        Ok(())
    }
}

/// Get the status code of the last HTTP request
/// Params: none
/// Returns: i32 (status code)
pub(crate) fn http_status_code(
    mut caller: Caller<Internal>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    output[0] = Val::I32(data.http_status as i32);
    Ok(())
}

/// Get the length of an allocated block given the offset
/// Params: i64 (offset)
/// Returns: i64 (length or 0)
pub(crate) fn length(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut Internal = caller.data_mut();
    let offset = args!(input, 0, i64) as usize;
    if offset == 0 {
        output[0] = Val::I64(0);
        return Ok(());
    }
    let length = match data.memory().block_length(offset) {
        Some(x) => x,
        None => return Err(Error::msg("Unable to find length for offset")),
    };
    output[0] = Val::I64(length as i64);
    Ok(())
}

pub fn log(
    level: log::Level,
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &Internal = caller.data();
    let offset = args!(input, 0, i64) as usize;
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
) -> Result<(), Error> {
    log(log::Level::Warn, caller, input, _output)
}

/// Write to logs (info)
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn log_info(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(log::Level::Info, caller, input, _output)
}

/// Write to logs (debug)
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn log_debug(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(log::Level::Debug, caller, input, _output)
}

/// Write to logs (error)
/// Params: i64 (offset)
/// Returns: none
pub(crate) fn log_error(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(log::Level::Error, caller, input, _output)
}
