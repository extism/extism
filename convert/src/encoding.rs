use crate::*;

use base64::Engine;

/// The `encoding` macro can be used to create newtypes that implement a particular encoding for the
/// inner value.
///
/// For example, the following line creates a new JSON encoding using serde_json:
///
/// ```
/// extism_convert::encoding!(MyJson, serde_json::to_vec, serde_json::from_slice);
/// ```
///
/// This will create a struct `struct MyJson<T>(pub T)` and implement `ToBytes` using `serde_json::to_vec`
/// and `FromBytesOwned` using `serde_json::from_vec`
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

        impl<T: serde::de::DeserializeOwned> $crate::FromBytesOwned for $name<T> {
            fn from_bytes_owned(data: &[u8]) -> std::result::Result<Self, $crate::Error> {
                let x = $from_slice(data)?;
                std::result::Result::Ok($name(x))
            }
        }

        impl<'a, T: serde::Serialize> $crate::ToBytes<'a> for $name<T> {
            type Bytes = Vec<u8>;

            fn to_bytes(&self) -> std::result::Result<Self::Bytes, $crate::Error> {
                let enc = $to_vec(&self.0)?;
                std::result::Result::Ok(enc)
            }
        }
    };
}

encoding!(Json, serde_json::to_vec, serde_json::from_slice);

#[cfg(feature = "msgpack")]
encoding!(Msgpack, rmp_serde::to_vec, rmp_serde::from_slice);

impl<'a> ToBytes<'a> for serde_json::Value {
    type Bytes = Vec<u8>;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(serde_json::to_vec(self)?)
    }
}

impl FromBytesOwned for serde_json::Value {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(serde_json::from_slice(data)?)
    }
}

/// Base64 conversion
///
/// When using `Base64` with `ToBytes` any type that implement `AsRef<[T]>` may be used as the inner value,
/// but only `Base64<String>` and `Base64<Vec>` may be used with `FromBytes`
///
/// A value wrapped in `Base64` will automatically be encoded/decoded using base64, the inner value should not
/// already be base64 encoded.
pub struct Base64<T: AsRef<[u8]>>(pub T);

impl<'a, T: AsRef<[u8]>> ToBytes<'a> for Base64<T> {
    type Bytes = String;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(base64::engine::general_purpose::STANDARD.encode(&self.0))
    }
}

impl FromBytesOwned for Base64<Vec<u8>> {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Base64(
            base64::engine::general_purpose::STANDARD.decode(data)?,
        ))
    }
}

impl FromBytesOwned for Base64<String> {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Base64(String::from_utf8(
            base64::engine::general_purpose::STANDARD.decode(data)?,
        )?))
    }
}

/// Protobuf encoding
///
/// Allows for `prost` Protobuf messages to be used as arguments to Extism plugin calls
#[cfg(feature = "protobuf")]
pub struct Protobuf<T: prost::Message>(pub T);

#[cfg(feature = "protobuf")]
impl<'a, T: prost::Message> ToBytes<'a> for Protobuf<T> {
    type Bytes = Vec<u8>;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.0.encode_to_vec())
    }
}

#[cfg(feature = "protobuf")]
impl<T: Default + prost::Message> FromBytesOwned for Protobuf<T> {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Protobuf(T::decode(data)?))
    }
}
