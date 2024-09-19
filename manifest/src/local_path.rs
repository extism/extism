use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LocalPath {
    ReadOnly(PathBuf),
    ReadWrite(PathBuf),
}

impl LocalPath {
    pub fn as_path(&self) -> &Path {
        match self {
            LocalPath::ReadOnly(p) => p.as_path(),
            LocalPath::ReadWrite(p) => p.as_path(),
        }
    }
}

impl From<&str> for LocalPath {
    fn from(value: &str) -> Self {
        if value.starts_with("ro:") {
            let s = &value[3..];
            LocalPath::ReadOnly(PathBuf::from(s))
        } else {
            LocalPath::ReadWrite(PathBuf::from(value))
        }
    }
}

impl From<String> for LocalPath {
    fn from(value: String) -> Self {
        LocalPath::from(value.as_str())
    }
}

impl From<PathBuf> for LocalPath {
    fn from(value: PathBuf) -> Self {
        LocalPath::ReadWrite(value)
    }
}

impl From<&Path> for LocalPath {
    fn from(value: &Path) -> Self {
        LocalPath::ReadWrite(value.to_path_buf())
    }
}

impl serde::Serialize for LocalPath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            LocalPath::ReadOnly(path) => {
                let s = match path.to_str() {
                    Some(s) => s,
                    None => {
                        return Err(serde::ser::Error::custom(
                            "Path contains invalid UTF-8 characters",
                        ))
                    }
                };

                format!("ro:{s}").serialize(serializer)
            }
            LocalPath::ReadWrite(path) => path.serialize(serializer),
        }
    }
}

struct LocalPathVisitor;

impl<'de> serde::de::Visitor<'de> for LocalPathVisitor {
    type Value = LocalPath;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("path string")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(From::from(v))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(From::from(v))
    }

    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        std::str::from_utf8(v)
            .map(From::from)
            .map_err(|_| serde::de::Error::invalid_value(serde::de::Unexpected::Bytes(v), &self))
    }

    fn visit_byte_buf<E>(self, v: Vec<u8>) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        String::from_utf8(v).map(From::from).map_err(|e| {
            serde::de::Error::invalid_value(serde::de::Unexpected::Bytes(&e.into_bytes()), &self)
        })
    }
}

impl<'de> serde::Deserialize<'de> for LocalPath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        deserializer.deserialize_string(LocalPathVisitor)
    }
}
