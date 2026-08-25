//! Extism 2 store extension.
//!
//! Addressable SQLite owned by the host. Plugins reach it through
//! `extism:host/store` (handles into kernel memory), not the kernel PDK.
//! Hosts seed and read guest paths with [`DurableStore::fs_put`] /
//! [`DurableStore::fs_get`] — there is no `allowed_paths` mount.
//!
//! Default backend is **in-memory**. A directory of files is opt-in for
//! operators who have a disk. Snapshot bytes ([`DurableStore::snapshot`] /
//! [`DurableStore::from_snapshot`]) are the hook for a host KV or remote
//! backend. This crate does not talk to the network.

use anyhow::{bail, Context, Result};
use rusqlite::hooks::{AuthAction, AuthContext, Authorization};
use rusqlite::{params, Connection, DatabaseName, OptionalExtension};
use serde_json::{json, Value};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

/// Import module name plugins use for this extension.
pub const STORE_MODULE: &str = "extism:host/store";

/// Default cap on the SQLite file/image size (64 MiB).
pub const DEFAULT_MAX_BYTES: u64 = 64 * 1024 * 1024;

/// Shared, addressable SQLite database for one Extism object id.
#[derive(Clone)]
pub struct DurableStore {
    inner: Arc<Inner>,
}

struct Inner {
    id: String,
    conn: Mutex<Connection>,
    max_bytes: u64,
}

/// Options for opening a store.
#[derive(Clone, Debug)]
pub struct StoreOptions {
    /// Object id (address). Becomes the filename stem for the disk backend.
    pub id: String,
    /// Maximum SQLite image size in bytes.
    pub max_bytes: u64,
}

impl Default for StoreOptions {
    fn default() -> Self {
        Self {
            id: "default".into(),
            max_bytes: DEFAULT_MAX_BYTES,
        }
    }
}

impl DurableStore {
    /// In-memory store. Survives `call`s on this handle; dies with the process.
    pub fn memory(id: impl Into<String>) -> Result<Self> {
        Self::memory_with(StoreOptions {
            id: id.into(),
            ..StoreOptions::default()
        })
    }

    pub fn memory_with(opts: StoreOptions) -> Result<Self> {
        let conn = Connection::open_in_memory().context("open in-memory sqlite")?;
        Self::from_conn(opts, conn)
    }

    /// SQLite file under `dir`, named `{sanitized_id}.sqlite`.
    ///
    /// Operators: this is local disk, one writer per id, WAL files included.
    /// Serverless hosts should use [`Self::memory`] or [`Self::from_snapshot`].
    pub fn open_dir(dir: impl AsRef<Path>, id: impl Into<String>) -> Result<Self> {
        Self::open_dir_with(
            dir,
            StoreOptions {
                id: id.into(),
                ..StoreOptions::default()
            },
        )
    }

    pub fn open_dir_with(dir: impl AsRef<Path>, opts: StoreOptions) -> Result<Self> {
        let dir = dir.as_ref();
        std::fs::create_dir_all(dir)
            .with_context(|| format!("create store dir {}", dir.display()))?;
        let stem = sanitize_id(&opts.id)?;
        let path: PathBuf = dir.join(format!("{stem}.sqlite"));
        let conn =
            Connection::open(&path).with_context(|| format!("open sqlite {}", path.display()))?;
        let _ = conn.pragma_update(None, "journal_mode", "WAL");
        Self::from_conn(opts, conn)
    }

    /// Hydrate from a snapshot produced by [`Self::snapshot`].
    pub fn from_snapshot(id: impl Into<String>, bytes: &[u8]) -> Result<Self> {
        Self::from_snapshot_with(
            StoreOptions {
                id: id.into(),
                ..StoreOptions::default()
            },
            bytes,
        )
    }

    pub fn from_snapshot_with(opts: StoreOptions, bytes: &[u8]) -> Result<Self> {
        let tmp = tmp_sqlite_path();
        std::fs::write(&tmp, bytes).context("write snapshot tempfile")?;
        let mut conn = Connection::open_in_memory().context("open snapshot sqlite")?;
        conn.restore(
            DatabaseName::Main,
            &tmp,
            None::<fn(rusqlite::backup::Progress)>,
        )
        .context("restore sqlite snapshot")?;
        let _ = std::fs::remove_file(&tmp);
        Self::from_conn(opts, conn)
    }

    fn from_conn(opts: StoreOptions, conn: Connection) -> Result<Self> {
        configure_connection(&conn)?;
        init_schema(&conn)?;
        Ok(Self {
            inner: Arc::new(Inner {
                id: opts.id,
                conn: Mutex::new(conn),
                max_bytes: opts.max_bytes,
            }),
        })
    }

    pub fn id(&self) -> &str {
        &self.inner.id
    }

    pub fn max_bytes(&self) -> u64 {
        self.inner.max_bytes
    }

    /// Serialize the database image (blob backend / remote hydrate).
    pub fn snapshot(&self) -> Result<Vec<u8>> {
        let tmp = tmp_sqlite_path();
        {
            let conn = self.inner.conn.lock().expect("store mutex");
            conn.backup(DatabaseName::Main, &tmp, None)
                .context("backup sqlite snapshot")?;
        }
        let bytes = std::fs::read(&tmp).context("read sqlite snapshot")?;
        let _ = std::fs::remove_file(&tmp);
        Ok(bytes)
    }

    /// Host/guest virtual filesystem: write `path` (must be absolute `/…`).
    pub fn fs_put(&self, path: &str, data: &[u8]) -> Result<()> {
        self.fs_put_mode(path, data, false)
    }

    /// Like [`Self::fs_put`] but subsequent guest `put`s to this path fail.
    pub fn fs_put_ro(&self, path: &str, data: &[u8]) -> Result<()> {
        self.fs_put_mode(path, data, true)
    }

    fn fs_put_mode(&self, path: &str, data: &[u8], readonly: bool) -> Result<()> {
        let path = normalize_guest_path(path)?;
        let conn = self.inner.conn.lock().expect("store mutex");
        let mtime = unix_now();
        conn.execute(
            "INSERT INTO extism_fs(path, data, readonly, mtime) VALUES (?1, ?2, ?3, ?4)
             ON CONFLICT(path) DO UPDATE SET
               data = excluded.data,
               readonly = excluded.readonly,
               mtime = excluded.mtime",
            params![path, data, readonly as i64, mtime],
        )?;
        drop(conn);
        self.check_size()
    }

    pub fn fs_get(&self, path: &str) -> Result<Option<Vec<u8>>> {
        let path = normalize_guest_path(path)?;
        let conn = self.inner.conn.lock().expect("store mutex");
        conn.query_row(
            "SELECT data FROM extism_fs WHERE path = ?1",
            params![path],
            |row| row.get(0),
        )
        .optional()
        .context("fs_get")
    }

    pub fn fs_exists(&self, path: &str) -> Result<bool> {
        Ok(self.fs_get(path)?.is_some())
    }

    pub fn fs_is_readonly(&self, path: &str) -> Result<bool> {
        let path = normalize_guest_path(path)?;
        let conn = self.inner.conn.lock().expect("store mutex");
        conn.query_row(
            "SELECT readonly FROM extism_fs WHERE path = ?1",
            params![path],
            |row| row.get::<_, i64>(0),
        )
        .optional()
        .map(|o| o.unwrap_or(0) != 0)
        .context("fs_is_readonly")
    }

    /// Guest `put`: fails if the path was seeded read-only by the host.
    pub fn fs_put_guest(&self, path: &str, data: &[u8]) -> Result<()> {
        if self.fs_is_readonly(path)? {
            bail!("path is read-only: {path}");
        }
        self.fs_put(path, data)
    }

    pub fn fs_delete(&self, path: &str) -> Result<bool> {
        let path = normalize_guest_path(path)?;
        if self.fs_is_readonly(&path)? {
            bail!("path is read-only: {path}");
        }
        let conn = self.inner.conn.lock().expect("store mutex");
        let n = conn.execute("DELETE FROM extism_fs WHERE path = ?1", params![path])?;
        Ok(n > 0)
    }

    pub fn fs_list(&self, prefix: &str) -> Result<Vec<String>> {
        let prefix = if prefix.is_empty() {
            "/".to_string()
        } else {
            normalize_guest_path(prefix)?
        };
        let conn = self.inner.conn.lock().expect("store mutex");
        let mut out = Vec::new();
        if prefix == "/" {
            let mut stmt = conn.prepare("SELECT path FROM extism_fs ORDER BY path")?;
            for r in stmt.query_map([], |row| row.get(0))? {
                out.push(r?);
            }
        } else {
            let mut stmt = conn.prepare(
                "SELECT path FROM extism_fs
                 WHERE path = ?1 OR path LIKE ?1 || '/%'
                 ORDER BY path",
            )?;
            for r in stmt.query_map(params![prefix], |row| row.get(0))? {
                out.push(r?);
            }
        }
        Ok(out)
    }

    /// Run one SQL statement. `params` is a JSON array of scalars (or `[]`).
    ///
    /// The object's database is private to this id. `ATTACH` and
    /// `load_extension` are denied. Result is a JSON value.
    pub fn exec(&self, sql: &str, params_json: &Value) -> Result<Value> {
        let sql = sql.trim();
        if sql.is_empty() {
            bail!("empty sql");
        }
        let bind = json_params(params_json)?;
        let conn = self.inner.conn.lock().expect("store mutex");
        let mut stmt = conn.prepare(sql).context("prepare sql")?;
        let col_count = stmt.column_count();
        let col_names: Vec<String> = stmt
            .column_names()
            .into_iter()
            .map(|s| s.to_string())
            .collect();

        if col_count == 0 {
            stmt.execute(rusqlite::params_from_iter(bind.iter()))
                .context("execute sql")?;
            let changes = conn.changes();
            drop(stmt);
            drop(conn);
            self.check_size()?;
            return Ok(json!({ "ok": true, "changes": changes }));
        }

        let mut rows = stmt
            .query(rusqlite::params_from_iter(bind.iter()))
            .context("query sql")?;
        let mut out_rows = Vec::new();
        while let Some(row) = rows.next()? {
            let mut obj = serde_json::Map::new();
            for (i, name) in col_names.iter().enumerate() {
                obj.insert(name.clone(), sqlite_to_json(row, i)?);
            }
            out_rows.push(Value::Object(obj));
        }
        Ok(json!({ "ok": true, "rows": out_rows }))
    }

    fn check_size(&self) -> Result<()> {
        let conn = self.inner.conn.lock().expect("store mutex");
        let page_count: i64 = conn.pragma_query_value(None, "page_count", |r| r.get(0))?;
        let page_size: i64 = conn.pragma_query_value(None, "page_size", |r| r.get(0))?;
        let used = (page_count as u64).saturating_mul(page_size as u64);
        if used > self.inner.max_bytes {
            bail!(
                "durable store exceeded max_bytes ({} > {})",
                used,
                self.inner.max_bytes
            );
        }
        Ok(())
    }
}

fn configure_connection(conn: &Connection) -> Result<()> {
    conn.authorizer(Some(deny_dangerous));
    let _ = conn.busy_timeout(std::time::Duration::from_secs(5));
    let _ = conn.pragma_update(None, "trusted_schema", "OFF");
    Ok(())
}

fn deny_dangerous(ctx: AuthContext<'_>) -> Authorization {
    match ctx.action {
        AuthAction::Attach { .. } => Authorization::Deny,
        AuthAction::Function { function_name }
            if function_name.eq_ignore_ascii_case("load_extension") =>
        {
            Authorization::Deny
        }
        AuthAction::Pragma { pragma_name, .. }
            if pragma_name.eq_ignore_ascii_case("load_extension") =>
        {
            Authorization::Deny
        }
        _ => Authorization::Allow,
    }
}

fn init_schema(conn: &Connection) -> Result<()> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS extism_fs (
            path TEXT PRIMARY KEY,
            data BLOB NOT NULL,
            readonly INTEGER NOT NULL DEFAULT 0,
            mtime INTEGER NOT NULL
         );",
    )?;
    Ok(())
}

fn tmp_sqlite_path() -> PathBuf {
    static N: AtomicU64 = AtomicU64::new(0);
    std::env::temp_dir().join(format!(
        "extism-store-{}-{}.sqlite",
        std::process::id(),
        N.fetch_add(1, Ordering::Relaxed)
    ))
}

fn unix_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

/// Guest paths are absolute Unix-style (`/foo/bar`). No `.` / `..` segments.
pub fn normalize_guest_path(path: &str) -> Result<String> {
    if path.is_empty() || path.contains('\0') {
        bail!("invalid store path");
    }
    if !path.starts_with('/') {
        bail!("store path must be absolute, got {path:?}");
    }
    if path.contains('\\') {
        bail!("store path must use '/' separators");
    }
    let mut out = Vec::new();
    for seg in path.split('/') {
        if seg.is_empty() {
            continue;
        }
        if seg == "." || seg == ".." {
            bail!("store path must not contain '.' or '..'");
        }
        if seg.len() > 255 {
            bail!("store path segment too long");
        }
        out.push(seg);
    }
    if out.is_empty() {
        return Ok("/".into());
    }
    let mut s = String::from("/");
    s.push_str(&out.join("/"));
    if s.len() > 4096 {
        bail!("store path too long");
    }
    Ok(s)
}

fn sanitize_id(id: &str) -> Result<String> {
    if id.is_empty() || id.len() > 128 {
        bail!("store id must be 1..=128 bytes");
    }
    if !id
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_' || c == '.')
    {
        bail!("store id may only contain [A-Za-z0-9._-]");
    }
    if id.starts_with('.') {
        bail!("store id must not start with '.'");
    }
    Ok(id.to_string())
}

fn json_params(v: &Value) -> Result<Vec<Bind>> {
    match v {
        Value::Null => Ok(vec![]),
        Value::Array(items) => items.iter().map(bind_value).collect(),
        other => bail!("sql params must be a JSON array, got {other}"),
    }
}

enum Bind {
    Null,
    Int(i64),
    Real(f64),
    Text(String),
}

impl rusqlite::types::ToSql for Bind {
    fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput<'_>> {
        use rusqlite::types::{ToSqlOutput, ValueRef};
        Ok(match self {
            Bind::Null => ToSqlOutput::Borrowed(ValueRef::Null),
            Bind::Int(i) => ToSqlOutput::Borrowed(ValueRef::Integer(*i)),
            Bind::Real(f) => ToSqlOutput::Borrowed(ValueRef::Real(*f)),
            Bind::Text(s) => ToSqlOutput::Borrowed(ValueRef::Text(s.as_bytes())),
        })
    }
}

fn bind_value(v: &Value) -> Result<Bind> {
    Ok(match v {
        Value::Null => Bind::Null,
        Value::Bool(b) => Bind::Int(i64::from(*b)),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Bind::Int(i)
            } else if let Some(f) = n.as_f64() {
                Bind::Real(f)
            } else {
                bail!("unsupported JSON number {n}");
            }
        }
        Value::String(s) => Bind::Text(s.clone()),
        Value::Array(_) | Value::Object(_) => bail!("sql params must be scalars, not {v}"),
    })
}

fn sqlite_to_json(row: &rusqlite::Row<'_>, i: usize) -> Result<Value> {
    use rusqlite::types::ValueRef;
    Ok(match row.get_ref(i)? {
        ValueRef::Null => Value::Null,
        ValueRef::Integer(n) => json!(n),
        ValueRef::Real(n) => json!(n),
        ValueRef::Text(t) => Value::String(String::from_utf8_lossy(t).into_owned()),
        ValueRef::Blob(b) => Value::String(format!("hex:{}", hex_encode(b))),
    })
}

fn hex_encode(bytes: &[u8]) -> String {
    const H: &[u8] = b"0123456789abcdef";
    let mut out = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        out.push(H[(b >> 4) as usize] as char);
        out.push(H[(b & 0xf) as usize] as char);
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_normalization() {
        assert_eq!(normalize_guest_path("/a/b").unwrap(), "/a/b");
        assert_eq!(normalize_guest_path("/").unwrap(), "/");
        assert!(normalize_guest_path("relative").is_err());
        assert!(normalize_guest_path("/a/../b").is_err());
        assert!(normalize_guest_path("/a/./b").is_err());
    }

    #[test]
    fn fs_roundtrip_and_readonly() {
        let s = DurableStore::memory("obj-1").unwrap();
        s.fs_put_ro("/data/hello.txt", b"hi").unwrap();
        assert_eq!(
            s.fs_get("/data/hello.txt").unwrap().as_deref(),
            Some(&b"hi"[..])
        );
        assert!(s.fs_put_guest("/data/hello.txt", b"nope").is_err());
        s.fs_put("/out/x", b"from-host").unwrap();
        assert_eq!(
            s.fs_list("/").unwrap(),
            vec!["/data/hello.txt".to_string(), "/out/x".to_string()]
        );
    }

    #[test]
    fn exec_and_snapshot() {
        let s = DurableStore::memory("obj-2").unwrap();
        s.exec("CREATE TABLE kv (k TEXT PRIMARY KEY, v TEXT)", &json!([]))
            .unwrap();
        s.exec("INSERT INTO kv(k,v) VALUES (?1, ?2)", &json!(["a", "b"]))
            .unwrap();
        let rows = s
            .exec("SELECT v FROM kv WHERE k = ?1", &json!(["a"]))
            .unwrap();
        assert_eq!(rows["rows"][0]["v"], "b");

        let snap = s.snapshot().unwrap();
        let s2 = DurableStore::from_snapshot("obj-2", &snap).unwrap();
        let rows = s2
            .exec("SELECT v FROM kv WHERE k = ?1", &json!(["a"]))
            .unwrap();
        assert_eq!(rows["rows"][0]["v"], "b");
    }

    #[test]
    fn attach_is_denied() {
        let s = DurableStore::memory("obj-3").unwrap();
        let err = s
            .exec("ATTACH DATABASE ':memory:' AS other", &json!([]))
            .unwrap_err();
        let msg = format!("{err:#}");
        assert!(
            msg.to_lowercase().contains("auth")
                || msg.to_lowercase().contains("denied")
                || msg.to_lowercase().contains("authorization"),
            "unexpected attach error: {msg}"
        );
    }

    #[test]
    fn load_extension_is_denied() {
        let s = DurableStore::memory("obj-ext").unwrap();
        let err = s
            .exec("SELECT load_extension('not-a-real-ext')", &json!([]))
            .unwrap_err();
        let msg = format!("{err:#}").to_lowercase();
        assert!(
            msg.contains("auth")
                || msg.contains("denied")
                || msg.contains("not authorized")
                || msg.contains("no such function")
                || msg.contains("load_extension"),
            "unexpected load_extension error: {msg}"
        );
    }

    #[test]
    fn disk_backend_roundtrip() {
        let dir = tempfile::tempdir().unwrap();
        let s = DurableStore::open_dir(dir.path(), "user-1").unwrap();
        s.fs_put("/x", b"disk").unwrap();
        drop(s);
        let s = DurableStore::open_dir(dir.path(), "user-1").unwrap();
        assert_eq!(s.fs_get("/x").unwrap().as_deref(), Some(&b"disk"[..]));
    }
}
