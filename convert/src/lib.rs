//! The `extism-convert` crate is used by the SDK and PDK to provide a shared interface for
//! encoding and decoding values that can be passed to Extism function calls.

pub use anyhow::Error;

/// `ToBytes` is used to define how a type should be encoded when working with
/// Extism memory. It is used for plugin input and host function output.
pub trait ToBytes<'a> {
    /// A configurable byte slice representation, allows any type that implements `AsRef<[u8]>`
    type Bytes: AsRef<[u8]>;

    /// `to_bytes` converts a value into `Self::Bytes`
    fn to_bytes(&'a self) -> Result<Self::Bytes, Error>;
}

/// `FromBytes` is used to define how a type should be decoded when working with
/// Extism memory. It is used for plugin output and host function input.
pub trait FromBytes<'a>: Sized {
    /// Decode a value from a slice of bytes
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error>;
}

impl<'a> ToBytes<'a> for Vec<u8> {
    type Bytes = &'a [u8];
    fn to_bytes(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self.as_slice())
    }
}

impl<'a> ToBytes<'a> for &'a [u8] {
    type Bytes = &'a [u8];
    fn to_bytes(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a> ToBytes<'a> for &'a str {
    type Bytes = &'a str;
    fn to_bytes(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a> ToBytes<'a> for String {
    type Bytes = &'a str;
    fn to_bytes(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self)
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

#[macro_export]
macro_rules! encoding {
    ($name:ident, $to_vec:expr, $from_slice:expr) => {
        pub struct $name<T>(pub T);

        impl<'a, T: serde::Deserialize<'a>> FromBytes<'a> for $name<T> {
            fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
                let x = $from_slice(data)?;
                Ok(Json(x))
            }
        }

        impl<'a, T: serde::Serialize> ToBytes<'a> for $name<T> {
            type Bytes = Vec<u8>;

            fn to_bytes(&'a self) -> Result<Self::Bytes, Error> {
                let enc = $to_vec(&self.0)?;
                Ok(enc)
            }
        }
    };
}

encoding!(Json, serde_json::to_vec, serde_json::from_slice);

#[cfg(feature = "msgpack")]
encoding!(Msgpack, rmp_serde::to_vec, rmp_serde::from_slice);
