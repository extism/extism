use crate::*;

macro_rules! plugin {
    (mut $a:expr) => {
        unsafe { (&mut *$a.plugin) }
    };

    ($a:expr) => {
        unsafe { (&*$a.plugin) }
    };
}

macro_rules! memory {
    (mut $a:expr) => {
        &mut plugin!(mut $a).memory
    };

    ($a:expr) => {
        &plugin!($a).memory
    };
}

pub(crate) fn input_length(
    caller: Caller<Internal>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    output[0] = Val::I64(data.input_length as i64);
    return Ok(());
}

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

pub(crate) fn alloc(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offs = memory!(mut data).alloc(input[0].unwrap_i64() as _)?;
    output[0] = Val::I64(offs.offset as i64);

    Ok(())
}

pub(crate) fn free(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offset = input[0].unwrap_i64() as usize;
    memory!(mut data).free(offset);
    Ok(())
}

pub(crate) fn store_u8(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let byte = input[1].unwrap_i32() as u8;
    memory!(mut data)
        .store_u8(input[0].unwrap_i64() as usize, byte)
        .map_err(|_| Trap::new("Write error"))?;
    Ok(())
}

pub(crate) fn load_u8(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let byte = memory!(data)
        .load_u8(input[0].unwrap_i64() as usize)
        .map_err(|_| Trap::new("Read error"))?;
    output[0] = Val::I32(byte as i32);
    Ok(())
}

pub(crate) fn store_u32(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let b = input[1].unwrap_i32() as u32;
    memory!(mut data)
        .store_u32(input[0].unwrap_i64() as usize, b)
        .map_err(|_| Trap::new("Write error"))?;
    Ok(())
}

pub(crate) fn load_u32(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let b = memory!(data)
        .load_u32(input[0].unwrap_i64() as usize)
        .map_err(|_| Trap::new("Read error"))?;
    output[0] = Val::I32(b as i32);
    Ok(())
}

pub(crate) fn store_u64(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let b = input[1].unwrap_i64() as u64;
    memory!(mut data)
        .store_u64(input[0].unwrap_i64() as usize, b)
        .map_err(|_| Trap::new("Write error"))?;
    Ok(())
}

pub(crate) fn load_u64(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let byte = memory!(data)
        .load_u64(input[0].unwrap_i64() as usize)
        .map_err(|_| Trap::new("Read error"))?;
    output[0] = Val::I64(byte as i64);
    Ok(())
}

pub(crate) fn error_set(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offset = input[0].unwrap_i64() as usize;
    let length = match memory!(data).block_length(offset) {
        Some(x) => x,
        None => return Err(Trap::new("Invalid offset in call to error_set")),
    };

    let handle = MemoryBlock { offset, length };
    if handle.offset == 0 {
        plugin!(mut data).clear_error();
        return Ok(());
    }

    let buf = memory!(data).get(handle);
    let s = unsafe { std::str::from_utf8_unchecked(buf) };
    plugin!(mut data).set_error(s);
    Ok(())
}

pub(crate) fn config_get(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offset = input[0].unwrap_i64() as usize;
    let length = match memory!(data).block_length(offset) {
        Some(x) => x,
        None => return Err(Trap::new("Invalid offset in call to config_get")),
    };

    let buf = memory!(data).get((offset, length));
    let str = unsafe { std::str::from_utf8_unchecked(buf) };
    let val = plugin!(data).manifest.as_ref().config.get(str);
    let mem = match val {
        Some(f) => memory!(mut data).alloc_bytes(f.as_bytes())?,
        None => {
            output[0] = Val::I64(0);
            return Ok(());
        }
    };

    output[0] = Val::I64(mem.offset as i64);
    Ok(())
}

pub(crate) fn var_get(
    mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();
    let offset = input[0].unwrap_i64() as usize;
    let length = match memory!(data).block_length(offset) {
        Some(x) => x,
        None => return Err(Trap::new("Invalid offset in call to var_get")),
    };

    let buf = memory!(data).get((offset, length));
    let str = unsafe { std::str::from_utf8_unchecked(buf) };
    let val = data.vars.get(str);
    let mem = match val {
        Some(f) => memory!(mut data).alloc_bytes(&f)?,
        None => {
            output[0] = Val::I64(0);
            return Ok(());
        }
    };

    output[0] = Val::I64(mem.offset as i64);
    Ok(())
}

pub(crate) fn var_set(
    mut caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &mut Internal = caller.data_mut();

    let mut size = 0;
    for v in data.vars.values() {
        size += v.len();
    }

    let voffset = input[1].unwrap_i64() as usize;

    // If the store is larger than 100MB then stop adding things
    if size > 1024 * 1024 * 100 && voffset != 0 {
        return Err(Trap::new("Variable store is full"));
    }

    let koffset = input[0].unwrap_i64() as usize;
    let klength = match memory!(data).block_length(koffset) {
        Some(x) => x,
        None => return Err(Trap::new("Invalid offset for key in call to var_set")),
    };

    let kbuf = memory!(data).get((koffset, klength));
    let kstr = unsafe { std::str::from_utf8_unchecked(kbuf) };

    if voffset == 0 {
        data.vars.remove(kstr);
        return Ok(());
    }

    let vlength = match memory!(data).block_length(voffset) {
        Some(x) => x,
        None => return Err(Trap::new("Invalid offset for value in call to var_set")),
    };

    let vbuf = memory!(data).get((voffset, vlength));

    data.vars.insert(kstr.to_string(), vbuf.to_vec());
    Ok(())
}

pub(crate) fn http_request(
    #[allow(unused_mut)] mut caller: Caller<Internal>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Trap> {
    #[cfg(not(feature = "http"))]
    {
        let _ = (caller, input, output);

        output[0] = Val::I64(0 as i64);
        error!("http_request is not enabled");
        return Ok(());
    }

    #[cfg(feature = "http")]
    {
        use std::io::Read;
        let data: &mut Internal = caller.data_mut();
        let offset = input[0].unwrap_i64() as usize;

        let length = match memory!(data).block_length(offset) {
            Some(x) => x,
            None => return Err(Trap::new("Invalid offset in call to http_request")),
        };
        let buf = memory!(data).get((offset, length));
        let req: extism_manifest::HttpRequest =
            serde_json::from_slice(buf).map_err(|_| Trap::new("Invalid http request"))?;

        let body_offset = input[1].unwrap_i64() as usize;

        let mut r = ureq::request(req.method.as_deref().unwrap_or("GET"), &req.url);

        for (k, v) in req.header.iter() {
            r = r.set(k, v);
        }

        let mut res = if body_offset > 0 {
            let length = match memory!(data).block_length(body_offset) {
                Some(x) => x,
                None => return Err(Trap::new("Invalid offset in call to http_request")),
            };
            let buf = memory!(data).get((offset, length));
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

        let mem = memory!(mut data).alloc_bytes(buf)?;

        output[0] = Val::I64(mem.offset as i64);
        Ok(())
    }
}

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
    let length = match memory!(data).block_length(offset) {
        Some(x) => x,
        None => return Err(Trap::new("Unable to find length for offset")),
    };
    output[0] = Val::I64(length as i64);
    Ok(())
}

pub(crate) fn log(
    level: log::Level,
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    let data: &Internal = caller.data();
    let offset = input[0].unwrap_i64() as usize;

    let length = match memory!(data).block_length(offset) {
        Some(x) => x,
        None => return Err(Trap::new("Invalid offset in call to http_request")),
    };
    let buf = memory!(data).get((offset, length));

    match std::str::from_utf8(buf) {
        Ok(buf) => log::log!(level, "{}", buf),
        Err(_) => log::log!(level, "{:?}", buf),
    }
    Ok(())
}

pub(crate) fn log_warn(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Warn, caller, input, _output)
}

pub(crate) fn log_info(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Info, caller, input, _output)
}

pub(crate) fn log_debug(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Debug, caller, input, _output)
}

pub(crate) fn log_error(
    caller: Caller<Internal>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Trap> {
    log(log::Level::Error, caller, input, _output)
}
