use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

#[deprecated]
pub type ManifestMemory = MemoryOptions;

#[derive(Default, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct MemoryOptions {
    #[serde(alias = "max")]
    pub max_pages: Option<u32>,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct HttpRequest {
    pub url: String,
    #[serde(default)]
    #[serde(alias = "header")]
    pub headers: std::collections::BTreeMap<String, String>,
    pub method: Option<String>,
}

impl HttpRequest {
    pub fn new(url: impl Into<String>) -> HttpRequest {
        HttpRequest {
            url: url.into(),
            headers: Default::default(),
            method: None,
        }
    }

    pub fn with_method(mut self, method: impl Into<String>) -> HttpRequest {
        self.method = Some(method.into());
        self
    }

    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> HttpRequest {
        self.headers.insert(key.into(), value.into());
        self
    }
}

#[derive(Default, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct WasmMetadata {
    pub name: Option<String>,
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

#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(untagged)]
#[serde(deny_unknown_fields)]
pub enum Wasm {
    File {
        path: PathBuf,
        #[serde(flatten)]
        meta: WasmMetadata,
    },
    Data {
        #[serde(with = "base64")]
        #[cfg_attr(feature = "json_schema", schemars(schema_with = "base64_schema"))]
        data: Vec<u8>,
        #[serde(flatten)]
        meta: WasmMetadata,
    },
    Url {
        #[serde(flatten)]
        req: HttpRequest,
        #[serde(flatten)]
        meta: WasmMetadata,
    },
}

impl Wasm {
    pub fn file(path: impl AsRef<std::path::Path>) -> Self {
        Wasm::File {
            path: path.as_ref().to_path_buf(),
            meta: Default::default(),
        }
    }

    pub fn data(data: impl Into<Vec<u8>>) -> Self {
        Wasm::Data {
            data: data.into(),
            meta: Default::default(),
        }
    }

    pub fn url(req: HttpRequest) -> Self {
        Wasm::Url {
            req,
            meta: Default::default(),
        }
    }

    pub fn meta(&self) -> &WasmMetadata {
        match self {
            Wasm::File { path: _, meta } => meta,
            Wasm::Data { data: _, meta } => meta,
            Wasm::Url { req: _, meta } => meta,
        }
    }

    pub fn meta_mut(&mut self) -> &mut WasmMetadata {
        match self {
            Wasm::File { path: _, meta } => meta,
            Wasm::Data { data: _, meta } => meta,
            Wasm::Url { req: _, meta } => meta,
        }
    }
}

#[cfg(feature = "json_schema")]
fn base64_schema(gen: &mut schemars::gen::SchemaGenerator) -> schemars::schema::Schema {
    use schemars::{schema::SchemaObject, JsonSchema};
    let mut schema: SchemaObject = <String>::json_schema(gen).into();
    schema.format = Some("string".to_owned());
    schema.into()
}

#[derive(Default, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(deny_unknown_fields)]
pub struct Manifest {
    #[serde(default)]
    pub wasm: Vec<Wasm>,
    #[serde(default)]
    pub memory: MemoryOptions,
    #[serde(default)]
    pub config: BTreeMap<String, String>,
    #[serde(default)]
    pub allowed_hosts: Option<Vec<String>>,
    #[serde(default)]
    pub allowed_paths: Option<BTreeMap<PathBuf, PathBuf>>,
    #[serde(default = "default_timeout")]
    pub timeout_ms: Option<u64>,
}

fn default_timeout() -> Option<u64> {
    Some(30000)
}

impl Manifest {
    /// Create a new manifest
    pub fn new(wasm: impl IntoIterator<Item = impl Into<Wasm>>) -> Manifest {
        Manifest {
            wasm: wasm.into_iter().map(|x| x.into()).collect(),
            timeout_ms: default_timeout(),
            ..Default::default()
        }
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
    pub fn with_allowed_path(mut self, src: impl AsRef<Path>, dest: impl AsRef<Path>) -> Self {
        let src = src.as_ref().to_path_buf();
        let dest = dest.as_ref().to_path_buf();
        match &mut self.allowed_paths {
            Some(p) => {
                p.insert(src, dest);
            }
            None => {
                let mut p = BTreeMap::new();
                p.insert(src, dest);
                self.allowed_paths = Some(p);
            }
        }

        self
    }

    /// Set `allowed_paths`
    pub fn with_allowed_paths(mut self, paths: impl Iterator<Item = (PathBuf, PathBuf)>) -> Self {
        self.allowed_paths = Some(paths.collect());
        self
    }

    /// Set `config`
    pub fn with_config(mut self, c: impl Iterator<Item = (String, String)>) -> Self {
        self.config = c.collect();
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

mod base64 {
    use base64::{engine::general_purpose, Engine as _};
    use serde::{Deserialize, Serialize};
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(v: &Vec<u8>, s: S) -> Result<S::Ok, S::Error> {
        let base64 = general_purpose::STANDARD.encode(v);
        String::serialize(&base64, s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<u8>, D::Error> {
        let base64 = String::deserialize(d)?;
        general_purpose::STANDARD
            .decode(base64.as_bytes())
            .map_err(serde::de::Error::custom)
    }
}
