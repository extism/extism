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
pub(crate) fn var_set(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();

    let mut size = 0;
    for v in data.vars.values() {
        size += v.len();
    }

    let voffset = args!(input, 1, i64) as u64;

    // If the store is larger than 100MB then stop adding things
    if size > 1024 * 1024 * 100 && voffset != 0 {
        return Err(Error::msg("Variable store is full"));
    }

    let key_offs = args!(input, 0, i64) as u64;
    let key = {
        let handle = match data.memory_handle(key_offs) {
            Some(h) => h,
            None => anyhow::bail!("invalid handle offset for var key: {key_offs}"),
        };
        let key = data.memory_str(handle)?;
        let key_len = key.len();
        let key_ptr = key.as_ptr();
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(key_ptr, key_len)) }
    };

    // Remove if the value offset is 0
    if voffset == 0 {
        data.vars.remove(key);
        return Ok(());
    }

    let handle = match data.memory_handle(voffset) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for var value: {voffset}"),
    };

    let value = data.memory_bytes(handle)?.to_vec();

    // Insert the value from memory into the `vars` map
    data.vars.insert(key.to_string(), value);

    Ok(())
}

/// Make an HTTP request
/// Params: i64 (offset to JSON encoded HttpRequest), i64 (offset to body or 0)
/// Returns: i64 (offset)
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
        let req: extism_manifest::HttpRequest = serde_json::from_slice(data.memory_bytes(handle)?)?;
        output[0] = Val::I64(0);
        anyhow::bail!(
            "http_request is not enabled, request to {} is not allowed",
            &req.url
        );
    }

    #[cfg(feature = "http")]
    {
        use std::io::Read;
        let handle = match data.memory_handle(http_req_offset) {
            Some(h) => h,
            None => anyhow::bail!("invalid handle offset for http request: {http_req_offset}"),
        };
        let req: extism_manifest::HttpRequest = serde_json::from_slice(data.memory_bytes(handle)?)?;

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

        let reader = match res {
            Ok(res) => {
                data.http_status = res.status();
                Some(res.into_reader())
            }
            Err(e) => {
                if let Some(res) = e.into_response() {
                    data.http_status = res.status();
                    Some(res.into_reader())
                } else {
                    None
                }
            }
        };

        if let Some(reader) = reader {
            let mut buf = Vec::new();
            reader
                .take(1024 * 1024 * 50) // TODO: make this limit configurable
                .read_to_end(&mut buf)?;

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

pub fn log(
    level: tracing::Level,
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let offset = args!(input, 0, i64) as u64;

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
    Ok(())
}

/// Write to logs (warning)
/// Params: i64 (offset)
/// Returns: none
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
pub(crate) fn log_error(
    caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    log(tracing::Level::ERROR, caller, input, _output)
}
