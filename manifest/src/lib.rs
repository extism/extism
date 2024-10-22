use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

mod local_path;

pub use local_path::LocalPath;

#[deprecated]
pub type ManifestMemory = MemoryOptions;

/// Configure memory settings
#[derive(Default, Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct MemoryOptions {
    /// The max number of WebAssembly pages that should be allocated
    pub max_pages: Option<u32>,

    /// The maximum number of bytes allowed in an HTTP response
    #[serde(default)]
    pub max_http_response_bytes: Option<u64>,

    /// The maximum number of bytes allowed to be used by plugin vars. Setting this to 0
    /// will disable Extism vars. The default value is 1mb.
    #[serde(default = "default_var_bytes")]
    pub max_var_bytes: Option<u64>,
}

impl MemoryOptions {
    /// Create an empty `MemoryOptions` value
    pub fn new() -> Self {
        Default::default()
    }

    /// Set max pages
    pub fn with_max_pages(mut self, pages: u32) -> Self {
        self.max_pages = Some(pages);
        self
    }

    /// Set max HTTP response size
    pub fn with_max_http_response_bytes(mut self, bytes: u64) -> Self {
        self.max_http_response_bytes = Some(bytes);
        self
    }

    /// Set max size of Extism vars
    pub fn with_max_var_bytes(mut self, bytes: u64) -> Self {
        self.max_var_bytes = Some(bytes);
        self
    }
}

fn default_var_bytes() -> Option<u64> {
    Some(1024 * 1024)
}

/// Generic HTTP request structure
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct HttpRequest {
    /// The request URL
    pub url: String,

    /// Request headers
    #[serde(default)]
    pub headers: std::collections::BTreeMap<String, String>,

    /// Request method
    pub method: Option<String>,
}

impl HttpRequest {
    /// Create a new `HttpRequest` to the given URL
    pub fn new(url: impl Into<String>) -> HttpRequest {
        HttpRequest {
            url: url.into(),
            headers: Default::default(),
            method: None,
        }
    }

    /// Update the method
    pub fn with_method(mut self, method: impl Into<String>) -> HttpRequest {
        self.method = Some(method.into());
        self
    }

    /// Add a header
    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> HttpRequest {
        self.headers.insert(key.into(), value.into());
        self
    }
}

/// Provides additional metadata about a Webassembly module
#[derive(Default, Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct WasmMetadata {
    /// Module name, this is used by Extism to determine which is the `main` module
    pub name: Option<String>,

    /// Module hash, if the data loaded from disk or via HTTP doesn't match an error will be raised
    pub hash: Option<String>,
}

impl From<HttpRequest> for Wasm {
    fn from(req: HttpRequest) -> Self {
        Wasm::Url {
            req,
            meta: WasmMetadata::default(),
        }
    }
}

impl From<std::path::PathBuf> for Wasm {
    fn from(path: std::path::PathBuf) -> Self {
        Wasm::File {
            path,
            meta: WasmMetadata::default(),
        }
    }
}

impl From<Vec<u8>> for Wasm {
    fn from(data: Vec<u8>) -> Self {
        Wasm::Data {
            data,
            meta: WasmMetadata::default(),
        }
    }
}

#[deprecated]
pub type ManifestWasm = Wasm;

/// The `Wasm` type specifies how to access a WebAssembly module
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(untagged)]
#[serde(deny_unknown_fields)]
pub enum Wasm {
    /// From disk
    File {
        path: PathBuf,
        #[serde(flatten)]
        meta: WasmMetadata,
    },

    /// From memory
    Data {
        #[serde(with = "wasmdata")]
        #[cfg_attr(feature = "json_schema", schemars(schema_with = "wasmdata_schema"))]
        data: Vec<u8>,
        #[serde(flatten)]
        meta: WasmMetadata,
    },

    /// Via HTTP
    Url {
        #[serde(flatten)]
        req: HttpRequest,
        #[serde(flatten)]
        meta: WasmMetadata,
    },
}

impl Wasm {
    /// Load Wasm from a path
    pub fn file(path: impl AsRef<std::path::Path>) -> Self {
        Wasm::File {
            path: path.as_ref().to_path_buf(),
            meta: Default::default(),
        }
    }

    /// Load Wasm directly from a buffer
    pub fn data(data: impl Into<Vec<u8>>) -> Self {
        Wasm::Data {
            data: data.into(),
            meta: Default::default(),
        }
    }

    /// Load Wasm from a URL
    pub fn url(url: impl Into<String>) -> Self {
        Wasm::Url {
            req: HttpRequest {
                url: url.into(),
                headers: Default::default(),
                method: None,
            },
            meta: Default::default(),
        }
    }

    /// Load Wasm from an HTTP request
    pub fn http(req: impl Into<HttpRequest>) -> Self {
        Wasm::Url {
            req: req.into(),
            meta: Default::default(),
        }
    }

    /// Get the metadata
    pub fn meta(&self) -> &WasmMetadata {
        match self {
            Wasm::File { path: _, meta } => meta,
            Wasm::Data { data: _, meta } => meta,
            Wasm::Url { req: _, meta } => meta,
        }
    }

    /// Get mutable access to the metadata
    pub fn meta_mut(&mut self) -> &mut WasmMetadata {
        match self {
            Wasm::File { path: _, meta } => meta,
            Wasm::Data { data: _, meta } => meta,
            Wasm::Url { req: _, meta } => meta,
        }
    }

    /// Update Wasm module name
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.meta_mut().name = Some(name.into());
        self
    }

    /// Update Wasm module hash
    pub fn with_hash(mut self, hash: impl Into<String>) -> Self {
        self.meta_mut().hash = Some(hash.into());
        self
    }
}

#[derive(Default, Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
struct DataPtrLength {
    ptr: u64,
    len: u64,
}

#[cfg(feature = "json_schema")]
fn wasmdata_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    use schemars::{schema::SchemaObject, JsonSchema};
    let mut schema: SchemaObject = <String>::json_schema(gen).into();
    let objschema: SchemaObject = <DataPtrLength>::json_schema(gen).into();
    let types = schemars::schema::SingleOrVec::<schemars::schema::InstanceType>::Vec(vec![
        schemars::schema::InstanceType::String,
        schemars::schema::InstanceType::Object,
    ]);
    schema.instance_type = Some(types);
    schema.object = objschema.object;
    schema.into()
}

/// The `Manifest` type is used to configure the runtime and specify how to load modules.
#[derive(Default, Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Manifest {
    /// WebAssembly modules, the `main` module should be named `main` or listed last
    #[serde(default)]
    pub wasm: Vec<Wasm>,

    /// Memory options
    #[serde(default)]
    pub memory: MemoryOptions,

    /// Config values are made accessible using the PDK `extism_config_get` function
    #[serde(default)]
    pub config: BTreeMap<String, String>,
    #[serde(default)]

    /// Specifies which hosts may be accessed via HTTP, if this is empty then
    /// no hosts may be accessed. Wildcards may be used.
    pub allowed_hosts: Option<Vec<String>>,

    /// Specifies which paths should be made available on disk when using WASI. This is a mapping from
    /// the path on disk to the path it should be available inside the plugin.
    /// For example, `".": "/tmp"` would mount the current directory as `/tmp` inside the module
    #[serde(default)]
    pub allowed_paths: Option<BTreeMap<LocalPath, PathBuf>>,

    /// The plugin timeout in milliseconds
    #[serde(default)]
    pub timeout_ms: Option<u64>,
}

impl Manifest {
    /// Create a new manifest
    pub fn new(wasm: impl IntoIterator<Item = impl Into<Wasm>>) -> Manifest {
        Manifest {
            wasm: wasm.into_iter().map(|x| x.into()).collect(),
            ..Default::default()
        }
    }

    pub fn with_wasm(mut self, wasm: impl Into<Wasm>) -> Self {
        self.wasm.push(wasm.into());
        self
    }

    /// Disallow HTTP requests to all hosts
    pub fn disallow_all_hosts(mut self) -> Self {
        self.allowed_hosts = Some(vec![]);
        self
    }

    /// Set memory options
    pub fn with_memory_options(mut self, memory: MemoryOptions) -> Self {
        self.memory = memory;
        self
    }

    /// Set MemoryOptions::memory_max
    pub fn with_memory_max(mut self, max: u32) -> Self {
        self.memory.max_pages = Some(max);
        self
    }

    /// Add a hostname to `allowed_hosts`
    pub fn with_allowed_host(mut self, host: impl Into<String>) -> Self {
        match &mut self.allowed_hosts {
            Some(h) => {
                h.push(host.into());
            }
            None => self.allowed_hosts = Some(vec![host.into()]),
        }

        self
    }

    /// Set `allowed_hosts`
    pub fn with_allowed_hosts(mut self, hosts: impl Iterator<Item = String>) -> Self {
        self.allowed_hosts = Some(hosts.collect());
        self
    }

    /// Add a path to `allowed_paths`
    pub fn with_allowed_path(mut self, src: impl Into<LocalPath>, dest: impl AsRef<Path>) -> Self {
        let dest = dest.as_ref().to_path_buf();
        match &mut self.allowed_paths {
            Some(p) => {
                p.insert(src.into(), dest);
            }
            None => {
                let mut p = BTreeMap::new();
                p.insert(src.into(), dest);
                self.allowed_paths = Some(p);
            }
        }

        self
    }

    /// Set `allowed_paths`
    pub fn with_allowed_paths(mut self, paths: impl Iterator<Item = (LocalPath, PathBuf)>) -> Self {
        self.allowed_paths = Some(paths.collect());
        self
    }

    /// Set `config`
    pub fn with_config(
        mut self,
        c: impl Iterator<Item = (impl Into<String>, impl Into<String>)>,
    ) -> Self {
        for (k, v) in c {
            self.config.insert(k.into(), v.into());
        }
        self
    }

    /// Set a single `config` key
    pub fn with_config_key(mut self, k: impl Into<String>, v: impl Into<String>) -> Self {
        self.config.insert(k.into(), v.into());
        self
    }

    /// Set `timeout_ms`, which will interrupt a plugin function's execution if it meets or
    /// exceeds this value. When an interrupt is made, the plugin will not be able to recover and
    /// continue execution.
    pub fn with_timeout(mut self, timeout: std::time::Duration) -> Self {
        self.timeout_ms = Some(timeout.as_millis() as u64);
        self
    }
}

mod wasmdata {
    use crate::DataPtrLength;
    use base64::{engine::general_purpose, Engine as _};
    use serde::{Deserialize, Serialize};
    use serde::{Deserializer, Serializer};
    use std::slice;

    pub fn serialize<S: Serializer>(v: &Vec<u8>, s: S) -> Result<S::Ok, S::Error> {
        let base64 = general_purpose::STANDARD.encode(v.as_slice());
        String::serialize(&base64, s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<u8>, D::Error> {
        #[derive(Deserialize)]
        #[serde(untagged)]
        enum WasmDataTypes {
            String(String),
            DataPtrLength(DataPtrLength),
        }
        Ok(match WasmDataTypes::deserialize(d)? {
            WasmDataTypes::String(string) => general_purpose::STANDARD
                .decode(string.as_bytes())
                .map_err(serde::de::Error::custom)?,
            WasmDataTypes::DataPtrLength(ptrlen) => {
                let slice =
                    unsafe { slice::from_raw_parts(ptrlen.ptr as *const u8, ptrlen.len as usize) };
                slice.to_vec()
            }
        })
    }
}

impl<'a> From<Manifest> for std::borrow::Cow<'a, [u8]> {
    fn from(m: Manifest) -> Self {
        let s = serde_json::to_vec(&m).unwrap();
        std::borrow::Cow::Owned(s)
    }
}

impl<'a> From<&Manifest> for std::borrow::Cow<'a, [u8]> {
    fn from(m: &Manifest) -> Self {
        let s = serde_json::to_vec(&m).unwrap();
        std::borrow::Cow::Owned(s)
    }
}
