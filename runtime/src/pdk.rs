/// All the functions in the file are exposed from inside WASM plugins
use crate::*;

/// This macro unwraps input arguments to prevent functions from panicking,
/// it should be used instead of `Val::unwrap_*` functions
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

/// Get a configuration value
/// Params: i64 (offset)
/// Returns: i64 (offset)
/// **Note**: this function takes ownership of the handle passed in
/// the caller should not `free` this value
pub(crate) fn config_get(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();

    let offset = args!(input, 0, i64) as u64;
    let handle = match data.memory_handle(offset) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for config key: {offset}"),
    };
    let key = data.memory_str(handle)?;
    let key = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(key.as_ptr(), key.len()))
    };
    let val = data.manifest.config.get(key);
    let ptr = val.map(|x| (x.len(), x.as_ptr()));
    data.memory_free(handle)?;
    let mem = match ptr {
        Some((len, ptr)) => {
            let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
            data.memory_new(bytes)?
        }
        None => {
            output[0] = Val::I64(0);
            return Ok(());
        }
    };
    output[0] = Val::I64(mem.offset() as i64);
    Ok(())
}

/// Get a variable
/// Params: i64 (offset)
/// Returns: i64 (offset)
/// **Note**: this function takes ownership of the handle passed in
/// the caller should not `free` this value, but the return value
/// will need to be freed
pub(crate) fn var_get(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();

    let offset = args!(input, 0, i64) as u64;
    let handle = match data.memory_handle(offset) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for var key: {offset}"),
    };
    let key = data.memory_str(handle)?;
    let key = unsafe {
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(key.as_ptr(), key.len()))
    };
    let val = data.vars.get(key);
    let ptr = val.map(|x| (x.len(), x.as_ptr()));
    data.memory_free(handle)?;

    let mem = match ptr {
        Some((len, ptr)) => {
            let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
            data.memory_new(bytes)?
        }
        None => {
            output[0] = Val::I64(0);
            return Ok(());
        }
    };
    output[0] = Val::I64(mem.offset() as i64);
    Ok(())
}

/// Set a variable, if the value offset is 0 then the provided key will be removed
/// Params: i64 (key offset), i64 (value offset)
/// Returns: none
/// **Note**: this function takes ownership of the handles passed in
/// the caller should not `free` these values
pub(crate) fn var_set(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();

    if data.manifest.memory.max_var_bytes.is_some_and(|x| x == 0) {
        anyhow::bail!("Vars are disabled by this host")
    }

    let voffset = args!(input, 1, i64) as u64;
    let key_offs = args!(input, 0, i64) as u64;

    let key_handle = match data.memory_handle(key_offs) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for var key: {key_offs}"),
    };
    let key = {
        let key = data.memory_str(key_handle)?;
        let key_len = key.len();
        let key_ptr = key.as_ptr();
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(key_ptr, key_len)) }
    };

    // Remove if the value offset is 0
    if voffset == 0 {
        data.vars.remove(key);
        data.memory_free(key_handle)?;
        return Ok(());
    }

    let handle = match data.memory_handle(voffset) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for var value: {voffset}"),
    };

    let mut size = std::mem::size_of::<String>()
        + std::mem::size_of::<Vec<u8>>()
        + key.len()
        + handle.length as usize;

    for (k, v) in data.vars.iter() {
        size += k.len();
        size += v.len();
        size += std::mem::size_of::<String>() + std::mem::size_of::<Vec<u8>>();
    }

    // If the store is larger than the configured size, or 1mb by default, then stop adding things
    if size > data.manifest.memory.max_var_bytes.unwrap_or(1024 * 1024) as usize && voffset != 0 {
        return Err(Error::msg("Variable store is full"));
    }

    let value = data.memory_bytes(handle)?.to_vec();

    data.memory_free(handle)?;
    data.memory_free(key_handle)?;

    // Insert the value from memory into the `vars` map
    data.vars.insert(key.to_string(), value);

    Ok(())
}

/// Make an HTTP request
/// Params: i64 (offset to JSON encoded HttpRequest), i64 (offset to body or 0)
/// Returns: i64 (offset)
/// **Note**: this function takes ownership of the handles passed in
/// the caller should not `free` these values, the result will need to
/// be freed.
pub(crate) fn http_request(
    #[allow(unused_mut)] mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let http_req_offset = args!(input, 0, i64) as u64;
    #[cfg(not(feature = "http"))]
    {
        let handle = match data.memory_handle(http_req_offset) {
            Some(h) => h,
            None => anyhow::bail!("http_request input is invalid: {http_req_offset}"),
        };
        data.memory_free(handle)?;
        let req: extism_manifest::HttpRequest = serde_json::from_slice(data.memory_bytes(handle)?)?;
        output[0] = Val::I64(0);
        anyhow::bail!(
            "http_request is not enabled, request to {} is not allowed",
            &req.url
        );
    }

    #[cfg(feature = "http")]
    {
        data.http_headers.iter_mut().for_each(|x| x.clear());
        data.http_status = 0;

        use std::io::Read;
        let handle = match data.memory_handle(http_req_offset) {
            Some(h) => h,
            None => anyhow::bail!("invalid handle offset for http request: {http_req_offset}"),
        };
        let req: extism_manifest::HttpRequest = serde_json::from_slice(data.memory_bytes(handle)?)?;
        data.memory_free(handle)?;

        let body_offset = args!(input, 1, i64) as u64;

        let url = match url::Url::parse(&req.url) {
            Ok(u) => u,
            Err(e) => return Err(Error::msg(format!("Invalid URL: {e:?}"))),
        };
        let allowed_hosts = &data.manifest.allowed_hosts;
        let host_str = url.host_str().unwrap_or_default();
        let host_matches = if let Some(allowed_hosts) = allowed_hosts {
            allowed_hosts.iter().any(|url| {
                let pat = match glob::Pattern::new(url) {
                    Ok(x) => x,
                    Err(_) => return url == host_str,
                };

                pat.matches(host_str)
            })
        } else {
            false
        };

        if !host_matches {
            return Err(Error::msg(format!(
                "HTTP request to {} is not allowed",
                req.url
            )));
        }

        let mut r = ureq::request(req.method.as_deref().unwrap_or("GET"), &req.url);

        for (k, v) in req.headers.iter() {
            r = r.set(k, v);
        }

        // Set HTTP timeout to respect the manifest timeout
        if let Some(remaining) = data.time_remaining() {
            r = r.timeout(remaining);
        }

        let res = if body_offset > 0 {
            let handle = match data.memory_handle(body_offset) {
                Some(h) => h,
                None => {
                    anyhow::bail!("invalid handle offset for http request body: {http_req_offset}")
                }
            };
            let buf: &[u8] = data.memory_bytes(handle)?;
            r.send_bytes(buf)
        } else {
            r.call()
        };

        if let Some(handle) = data.memory_handle(body_offset) {
            data.memory_free(handle)?;
        }

        let reader = match res {
            Ok(res) => {
                if let Some(headers) = &mut data.http_headers {
                    for name in res.headers_names() {
                        if let Some(h) = res.header(&name) {
                            headers.insert(name, h.to_string());
                        }
                    }
                }
                data.http_status = res.status();
                Some(res.into_reader())
            }
            Err(e) => {
                // Catch timeout and return
                if let Some(d) = data.time_remaining() {
                    if e.kind() == ureq::ErrorKind::Io && d.as_nanos() == 0 {
                        anyhow::bail!("timeout");
                    }
                }
                let msg = e.to_string();
                if let Some(res) = e.into_response() {
                    data.http_status = res.status();
                    Some(res.into_reader())
                } else {
                    return Err(Error::msg(msg));
                }
            }
        };

        if let Some(reader) = reader {
            let mut buf = Vec::new();
            let max = if let Some(max) = &data.manifest.memory.max_http_response_bytes {
                reader.take(*max + 1).read_to_end(&mut buf)?;
                *max
            } else {
                reader.take(1024 * 1024 * 50 + 1).read_to_end(&mut buf)?;
                1024 * 1024 * 50
            };

            if buf.len() > max as usize {
                anyhow::bail!("HTTP response exceeds the configured maximum number of bytes: {max}")
            }

            let mem = data.memory_new(&buf)?;
            output[0] = Val::I64(mem.offset() as i64);
        } else {
            output[0] = Val::I64(0);
        }

        Ok(())
    }
}

/// Get the status code of the last HTTP request
/// Params: none
/// Returns: i32 (status code)
pub(crate) fn http_status_code(
    mut caller: Caller<CurrentPlugin>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    output[0] = Val::I32(data.http_status as i32);
    Ok(())
}

/// Get the HTTP response headers from the last HTTP request
/// Params: none
/// Returns: i64 (offset)
pub(crate) fn http_headers(
    mut caller: Caller<CurrentPlugin>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    if let Some(h) = &data.http_headers {
        let headers = serde_json::to_string(h)?;
        data.memory_set_val(&mut output[0], headers)?;
    } else {
        output[0] = Val::I64(0);
    }
    Ok(())
}

pub fn log(
    level: tracing::Level,
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();

    let offset = args!(input, 0, i64) as u64;

    // Check if the current log level should be logged
    let global_log_level = tracing::level_filters::LevelFilter::current();
    if global_log_level == tracing::level_filters::LevelFilter::OFF || level > global_log_level {
        if let Some(handle) = data.memory_handle(offset) {
            data.memory_free(handle)?;
        }
        return Ok(());
    }

    let handle = match data.memory_handle(offset) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for log message: {offset}"),
    };
    let id = data.id.to_string();
    let buf = data.memory_str(handle);

    match buf {
        Ok(buf) => match level {
            tracing::Level::ERROR => {
                tracing::error!(plugin = id, "{}", buf)
            }
            tracing::Level::DEBUG => {
                tracing::debug!(plugin = id, "{}", buf)
            }
            tracing::Level::WARN => {
                tracing::warn!(plugin = id, "{}", buf)
            }
            tracing::Level::INFO => {
                tracing::info!(plugin = id, "{}", buf)
            }
            tracing::Level::TRACE => {
                tracing::trace!(plugin = id, "{}", buf)
            }
        },
        Err(_) => tracing::error!(plugin = id, "unable to log message: {:?}", buf),
    }

    data.memory_free(handle)?;
    Ok(())
}

/// Write to logs (warning)
/// Params: i64 (offset)
/// Returns: none
/// **Note**: this function takes ownership of the handle passed in
/// the caller should not `free` this value
pub(crate) fn log_warn(
    caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(tracing::Level::WARN, caller, input, _output)
}

/// Write to logs (info)
/// Params: i64 (offset)
/// Returns: none
/// **Note**: this function takes ownership of the handle passed in
/// the caller should not `free` this value
pub(crate) fn log_info(
    caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(tracing::Level::INFO, caller, input, _output)
}

/// Write to logs (debug)
/// Params: i64 (offset)
/// Returns: none
/// **Note**: this function takes ownership of the handle passed in
/// the caller should not `free` this value
pub(crate) fn log_debug(
    caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(tracing::Level::DEBUG, caller, input, _output)
}

/// Write to logs (error)
/// Params: i64 (offset)
/// Returns: none
/// **Note**: this function takes ownership of the handle passed in
/// the caller should not `free` this value
pub(crate) fn log_error(
    caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(tracing::Level::ERROR, caller, input, _output)
}

/// Write to logs (trace)
/// Params: i64 (offset)
/// Returns: none
/// **Note**: this function takes ownership of the handle passed in
/// the caller should not `free` this value
pub(crate) fn log_trace(
    caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(tracing::Level::TRACE, caller, input, _output)
}

/// Get the log level
/// Params: none
/// Returns: i32 (log level)
pub(crate) fn get_log_level(
    mut _caller: Caller<CurrentPlugin>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let level = tracing::level_filters::LevelFilter::current();
    if level == tracing::level_filters::LevelFilter::OFF {
        output[0] = Val::I32(i32::MAX)
    } else {
        output[0] = Val::I32(log_level_to_int(
            level.into_level().unwrap_or(tracing::Level::ERROR),
        ));
    }
    Ok(())
}

/// Convert log level to integer
pub(crate) const fn log_level_to_int(level: tracing::Level) -> i32 {
    match level {
        tracing::Level::TRACE => 0,
        tracing::Level::DEBUG => 1,
        tracing::Level::INFO => 2,
        tracing::Level::WARN => 3,
        tracing::Level::ERROR => 4,
    }
}
