//! The [extism-convert](https://crates.io/crates/extism-convert) crate is used by the [Rust SDK](https://crates.io/crates/extism) and [Rust PDK](https://crates.io/crates/extism-pdk) to provide a shared interface for
//! encoding and decoding values that can be passed to Extism function calls.
//!
//! A set of types (Json, Msgpack) that can be used to specify a serde encoding are also provided. These are
//! similar to [axum extractors](https://docs.rs/axum/latest/axum/extract/index.html#intro) - they are
//! implemented as a tuple struct with a single field that is meant to be extracted using pattern matching.

pub use anyhow::Error;

mod encoding;

mod from_bytes;
mod memory_handle;
mod to_bytes;

pub use encoding::{Base64, Json};

#[cfg(feature = "msgpack")]
pub use encoding::Msgpack;

#[cfg(feature = "prost")]
pub use encoding::Prost;

#[cfg(feature = "protobuf")]
pub use encoding::Protobuf;

#[cfg(all(feature = "raw", target_endian = "little"))]
pub use encoding::Raw;

pub use from_bytes::{FromBytes, FromBytesOwned};
pub use memory_handle::MemoryHandle;
pub use to_bytes::ToBytes;

#[cfg(test)]
mod tests;
