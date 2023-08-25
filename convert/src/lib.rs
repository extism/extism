//! The `extism-convert` crate is used by the SDK and PDK to provide a shared interface for
//! encoding and decoding values that can be passed to Extism function calls.
//!
//! A set of types (Json, Msgpack) that can be used to specify a serde encoding are also provided. These are
//! similar to [axum extractors](https://docs.rs/axum/latest/axum/extract/index.html#intro) - they are
//! implemented as a tuple struct with a single field that is meant to be extracted using pattern matching.

pub use anyhow::Error;

/// `MemoryHandle` describes where in memory a block of data is stored
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub struct MemoryHandle {
    pub offset: u64,
    pub length: u64,
}

impl MemoryHandle {
    /// Create a new `MemoryHandle` from an offset in memory and length
    ///
    /// # Safety
    /// This function is unsafe because the specified memory region may not be valid.
    pub unsafe fn new(offset: u64, length: u64) -> MemoryHandle {
        MemoryHandle { offset, length }
    }

    /// `NULL` equivalent
    pub fn null() -> MemoryHandle {
        MemoryHandle {
            offset: 0,
            length: 0,
        }
    }

    /// Get the offset of a memory handle
    pub fn offset(&self) -> u64 {
        self.offset
    }

    /// Get the length of the memory region
    pub fn len(&self) -> usize {
        self.length as usize
    }

    /// Returns `true` when the length is 0
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
}

/// `ToBytes` is used to define how a type should be encoded when working with
/// Extism memory. It is used for plugin input and host function output.
pub trait ToBytes<'a> {
    /// A configurable byte slice representation, allows any type that implements `AsRef<[u8]>`
    type Bytes: AsRef<[u8]>;

    /// `to_bytes` converts a value into `Self::Bytes`
    fn to_bytes(&self) -> Result<Self::Bytes, Error>;
}

/// `FromBytes` is used to define how a type should be decoded when working with
/// Extism memory. It is used for plugin output and host function input.
pub trait FromBytes<'a>: Sized {
    /// Decode a value from a slice of bytes
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error>;
}

impl<'a> ToBytes<'a> for &'a Vec<u8> {
    type Bytes = &'a [u8];
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.as_slice())
    }
}

impl<'a> ToBytes<'a> for &'a [u8] {
    type Bytes = &'a [u8];
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a> ToBytes<'a> for &'a str {
    type Bytes = &'a str;
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a> ToBytes<'a> for &'a String {
    type Bytes = &'a str;
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a> ToBytes<'a> for f64 {
    type Bytes = [u8; 8];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl<'a> ToBytes<'a> for f32 {
    type Bytes = [u8; 4];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl<'a> FromBytes<'a> for &'a [u8] {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        Ok(data)
    }
}

impl<'a> FromBytes<'a> for &'a str {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        Ok(std::str::from_utf8(data)?)
    }
}

impl<'a> FromBytes<'a> for Vec<u8> {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        Ok(data.to_vec())
    }
}

impl<'a> FromBytes<'a> for String {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        Ok(std::str::from_utf8(data)?.to_string())
    }
}

impl<'a> FromBytes<'a> for f64 {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

impl<'a> FromBytes<'a> for f32 {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

#[macro_export]
macro_rules! encoding {
    ($name:ident, $to_vec:expr, $from_slice:expr) => {
        #[doc = concat!(stringify!($name), " encoding")]
        pub struct $name<T>(pub T);

        impl<T> $name<T> {
            pub fn into_inner(self) -> T {
                self.0
            }
        }

        impl<'a, T: serde::Deserialize<'a>> FromBytes<'a> for $name<T> {
            fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
                let x = $from_slice(data)?;
                Ok($name(x))
            }
        }

        impl<'a, T: serde::Serialize> ToBytes<'a> for &'a $name<T> {
            type Bytes = Vec<u8>;

            fn to_bytes(&self) -> Result<Self::Bytes, Error> {
                let enc = $to_vec(&self.0)?;
                Ok(enc)
            }
        }
    };
}

encoding!(Json, serde_json::to_vec, serde_json::from_slice);

#[cfg(feature = "msgpack")]
encoding!(Msgpack, rmp_serde::to_vec, rmp_serde::from_slice);
