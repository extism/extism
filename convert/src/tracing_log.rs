use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Default, Deserialize, Serialize)]
#[serde(default)]
pub struct TracingLog {
    /// Additional data to be rendered as fields.
    /// https://docs.rs/tracing/latest/tracing/#recording-fields
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub fields: HashMap<String, String>,

    /// The log message.
    pub message: String,

    /// The target/model the log comes from.
    /// https://docs.rs/tracing/latest/tracing/#configuring-attributes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
}
