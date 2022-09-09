use std::collections::BTreeMap;

#[derive(Default, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
pub struct ManifestMemory {
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

#[derive(serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "json_schema", derive(schemars::JsonSchema))]
#[serde(untagged)]
pub enum ManifestWasm {
    File {
        path: std::path::PathBuf,
        name: Option<String>,
        hash: Option<String>,
    },
    Data {
        #[serde(with = "base64")]
        #[cfg_attr(feature = "json_schema", schemars(schema_with = "base64_schema"))]
        data: Vec<u8>,
        name: Option<String>,
        hash: Option<String>,
    },
    Url {
        #[serde(flatten)]
        req: HttpRequest,
        name: Option<String>,
        hash: Option<String>,
    },
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
    pub wasm: Vec<ManifestWasm>,
    #[serde(default)]
    pub memory: ManifestMemory,
    #[serde(default)]
    pub config: BTreeMap<String, String>,
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
        base64::decode(base64.as_bytes()).map_err(|e| serde::de::Error::custom(e))
    }
}
