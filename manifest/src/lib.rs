use std::collections::BTreeMap;

#[derive(Default, serde::Serialize, serde::Deserialize)]
pub struct ManifestMemory {
    pub max: Option<u32>,
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
pub enum ManifestWasm {
    File {
        path: std::path::PathBuf,
        name: Option<String>,
        hash: Option<String>,
    },
    Data {
        #[serde(with = "base64")]
        data: Vec<u8>,
        name: Option<String>,
        hash: Option<String>,
    },
    Url {
        url: String,
        #[serde(default)]
        header: BTreeMap<String, String>,
        name: Option<String>,
        method: Option<String>,
        hash: Option<String>,
    },
}

#[derive(Default, serde::Serialize, serde::Deserialize)]
pub struct Manifest {
    #[serde(default = "Vec::new")]
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
