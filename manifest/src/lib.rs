use std::collections::BTreeMap;

#[deprecated]
pub type ManifestMemory = MemoryOptions;

#[derive(Default, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
pub struct MemoryOptions {
    #[serde(alias = "max_pages")]
    pub max: Option<u32>,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
pub struct HttpRequest {
    pub url: String,
    #[serde(default)]
    pub header: std::collections::BTreeMap<String, String>,
    pub method: Option<String>,
}

impl HttpRequest {
    pub fn new(url: impl Into<String>) -> HttpRequest {
        HttpRequest {
            url: url.into(),
            header: Default::default(),
            method: None,
        }
    }

    pub fn with_method(mut self, method: impl Into<String>) -> HttpRequest {
        self.method = Some(method.into());
        self
    }

    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> HttpRequest {
        self.header.insert(key.into(), value.into());
        self
    }
}

#[derive(Default, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
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
pub enum Wasm {
    File {
        path: std::path::PathBuf,

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
            Wasm::File { path: _, meta } => &meta,
            Wasm::Data { data: _, meta } => &meta,
            Wasm::Url { req: _, meta } => &meta,
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
pub struct Manifest {
    #[serde(default)]
    pub wasm: Vec<Wasm>,
    #[serde(default)]
    pub memory: MemoryOptions,
    #[serde(default)]
    pub config: BTreeMap<String, String>,
    #[serde(default)]
    pub allowed_hosts: Option<Vec<String>>,
}

impl Manifest {
    /// Create a new manifest
    pub fn new(wasm: impl Into<Vec<Wasm>>) -> Manifest {
        Manifest {
            wasm: wasm.into(),
            ..Default::default()
        }
    }
}

mod base64 {
    use serde::{Deserialize, Serialize};
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(v: &Vec<u8>, s: S) -> Result<S::Ok, S::Error> {
        let base64 = base64::encode(v);
        String::serialize(&base64, s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<u8>, D::Error> {
        let base64 = String::deserialize(d)?;
        base64::decode(base64.as_bytes()).map_err(serde::de::Error::custom)
    }
}
