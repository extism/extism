//! Extism 2 store extension (`extism:host/store`).
//!
//! Separate from the kernel PDK (`extism:host/env`). The host owns SQLite via
//! [`extism_store::DurableStore`]; plugins only see kernel handles. Linked as
//! ordinary host functions so Wasmtime and a future Wasm3 `Plugin` share one ABI.

use crate::*;

macro_rules! args {
    ($input:expr, $index:expr, $ty:ident) => {
        match $input[$index].$ty() {
            Some(x) => x,
            None => return Err($crate::Error::msg("Invalid input type")),
        }
    };
}

pub(crate) fn link(engine: &Engine, linker: &mut Linker<CurrentPlugin>) -> Result<(), Error> {
    use wasmtime::ValType::I64;
    let get_t = FuncType::new(engine, [I64], [I64]);
    linker.func_new(EXTISM_STORE_MODULE, "get", get_t, |c, i, o| {
        store_get(c, i, o).to_wasmtime_result()
    })?;
    let put_t = FuncType::new(engine, [I64, I64], []);
    linker.func_new(EXTISM_STORE_MODULE, "put", put_t, |c, i, o| {
        store_put(c, i, o).to_wasmtime_result()
    })?;
    let del_t = FuncType::new(engine, [I64], []);
    linker.func_new(EXTISM_STORE_MODULE, "delete", del_t, |c, i, o| {
        store_delete(c, i, o).to_wasmtime_result()
    })?;
    let list_t = FuncType::new(engine, [I64], [I64]);
    linker.func_new(EXTISM_STORE_MODULE, "list", list_t, |c, i, o| {
        store_list(c, i, o).to_wasmtime_result()
    })?;
    let exec_t = FuncType::new(engine, [I64, I64], [I64]);
    linker.func_new(EXTISM_STORE_MODULE, "exec", exec_t, |c, i, o| {
        store_exec(c, i, o).to_wasmtime_result()
    })?;
    let id_t = FuncType::new(engine, [], [I64]);
    linker.func_new(EXTISM_STORE_MODULE, "id", id_t, |c, i, o| {
        store_id(c, i, o).to_wasmtime_result()
    })?;
    Ok(())
}

fn require_store(data: &CurrentPlugin) -> Result<extism_store::DurableStore, Error> {
    data.durable
        .clone()
        .ok_or_else(|| Error::msg("durable store is not configured"))
}

/// `extism:host/store::get` — path handle in, value handle out (0 if missing).
fn store_get(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let store = require_store(data)?;
    let offset = args!(input, 0, i64) as u64;
    let handle = match data.memory_handle(offset) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for store path: {offset}"),
    };
    let path = data.memory_str(handle)?.to_string();
    data.memory_free(handle)?;
    match store.fs_get(&path)? {
        Some(bytes) => {
            let mem = data.memory_new(bytes)?;
            output[0] = Val::I64(mem.offset() as i64);
        }
        None => {
            output[0] = Val::I64(0);
        }
    }
    Ok(())
}

/// `extism:host/store::put` — path handle, data handle.
fn store_put(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let store = require_store(data)?;
    let path_off = args!(input, 0, i64) as u64;
    let data_off = args!(input, 1, i64) as u64;
    let path_handle = match data.memory_handle(path_off) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for store path: {path_off}"),
    };
    let path = data.memory_str(path_handle)?.to_string();
    let body = if data_off == 0 {
        Vec::new()
    } else {
        let h = match data.memory_handle(data_off) {
            Some(h) => h,
            None => anyhow::bail!("invalid handle offset for store data: {data_off}"),
        };
        let bytes = data.memory_bytes(h)?.to_vec();
        data.memory_free(h)?;
        bytes
    };
    data.memory_free(path_handle)?;
    store.fs_put_guest(&path, &body)?;
    Ok(())
}

/// `extism:host/store::delete`
fn store_delete(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    _output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let store = require_store(data)?;
    let offset = args!(input, 0, i64) as u64;
    let handle = match data.memory_handle(offset) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for store path: {offset}"),
    };
    let path = data.memory_str(handle)?.to_string();
    data.memory_free(handle)?;
    store.fs_delete(&path)?;
    Ok(())
}

/// `extism:host/store::list` — prefix handle in, JSON array of paths out.
fn store_list(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let store = require_store(data)?;
    let offset = args!(input, 0, i64) as u64;
    let prefix = if offset == 0 {
        "/".to_string()
    } else {
        let handle = match data.memory_handle(offset) {
            Some(h) => h,
            None => anyhow::bail!("invalid handle offset for store prefix: {offset}"),
        };
        let p = data.memory_str(handle)?.to_string();
        data.memory_free(handle)?;
        p
    };
    let listed = store.fs_list(&prefix)?;
    let json = serde_json::to_vec(&listed)?;
    let mem = data.memory_new(json)?;
    output[0] = Val::I64(mem.offset() as i64);
    Ok(())
}

/// `extism:host/store::exec` — SQL handle, JSON-params handle (0 = `[]`).
fn store_exec(
    mut caller: Caller<CurrentPlugin>,
    input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let store = require_store(data)?;
    let sql_off = args!(input, 0, i64) as u64;
    let params_off = args!(input, 1, i64) as u64;
    let sql_handle = match data.memory_handle(sql_off) {
        Some(h) => h,
        None => anyhow::bail!("invalid handle offset for sql: {sql_off}"),
    };
    let sql = data.memory_str(sql_handle)?.to_string();
    data.memory_free(sql_handle)?;
    let params = if params_off == 0 {
        serde_json::json!([])
    } else {
        let h = match data.memory_handle(params_off) {
            Some(h) => h,
            None => anyhow::bail!("invalid handle offset for sql params: {params_off}"),
        };
        let v: serde_json::Value = serde_json::from_slice(data.memory_bytes(h)?)?;
        data.memory_free(h)?;
        v
    };
    let result = store.exec(&sql, &params)?;
    let json = serde_json::to_vec(&result)?;
    let mem = data.memory_new(json)?;
    output[0] = Val::I64(mem.offset() as i64);
    Ok(())
}

/// `extism:host/store::id`
fn store_id(
    mut caller: Caller<CurrentPlugin>,
    _input: &[Val],
    output: &mut [Val],
) -> Result<(), Error> {
    let data: &mut CurrentPlugin = caller.data_mut();
    let store = require_store(data)?;
    let mem = data.memory_new(store.id())?;
    output[0] = Val::I64(mem.offset() as i64);
    Ok(())
}
