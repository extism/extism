use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Default, Deserialize, Serialize)]
#[serde(default)]
pub struct TracingEvent {
    /// Additional data to be rendered as fields.
    /// https://docs.rs/tracing/latest/tracing/#recording-fields
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub fields: HashMap<String, String>,

    /// The log message.
    pub message: String,

    /// The target/module the log comes from.
    /// https://docs.rs/tracing/latest/tracing/#configuring-attributes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
}

impl TracingEvent {
    pub fn new(message: impl AsRef<str>) -> Self {
        Self {
            message: message.as_ref().into(),
            ..Default::default()
        }
    }

    pub fn field(&mut self, key: impl AsRef<str>, value: impl AsRef<str>) -> &mut Self {
        self.fields
            .insert(key.as_ref().into(), value.as_ref().into());
        self
    }

    pub fn target(&mut self, value: impl AsRef<str>) -> &mut Self {
        self.target = Some(value.as_ref().into());
        self
    }
}

impl FromStr for TracingEvent {
    type Err = ();

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(value))
    }
}

impl From<&str> for TracingEvent {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl From<String> for TracingEvent {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}
