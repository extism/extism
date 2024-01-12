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
    ($pub:vis $name:ident, $to_vec:expr, $from_slice:expr) => {
        #[doc = concat!(stringify!($name), " encoding")]
        #[derive(Debug)]
        $pub struct $name<T>(pub T);

        impl<T> $name<T> {
            pub fn into_inner(self) -> T {
                self.0
            }
        }

        impl<T> From<T> for $name<T> {
            fn from(data: T) -> Self {
                Self(data)
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

encoding!(pub Json, serde_json::to_vec, serde_json::from_slice);

#[cfg(feature = "msgpack")]
encoding!(pub Msgpack, rmp_serde::to_vec, rmp_serde::from_slice);

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
#[derive(Debug)]
pub struct Base64<T: AsRef<[u8]>>(pub T);

impl<T: AsRef<[u8]>> From<T> for Base64<T> {
    fn from(data: T) -> Self {
        Self(data)
    }
}

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
#[cfg(feature = "prost")]
#[derive(Debug)]
pub struct Prost<T: prost::Message>(pub T);

#[cfg(feature = "prost")]
impl<T: prost::Message> From<T> for Prost<T> {
    fn from(data: T) -> Self {
        Self(data)
    }
}

#[cfg(feature = "prost")]
impl<'a, T: prost::Message> ToBytes<'a> for Prost<T> {
    type Bytes = Vec<u8>;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.0.encode_to_vec())
    }
}

#[cfg(feature = "prost")]
impl<T: Default + prost::Message> FromBytesOwned for Prost<T> {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Prost(T::decode(data)?))
    }
}

/// Protobuf encoding
///
/// Allows for `rust-protobuf` Protobuf messages to be used as arguments to Extism plugin calls
#[cfg(feature = "protobuf")]
pub struct Protobuf<T: protobuf::Message>(pub T);

#[cfg(feature = "protobuf")]
impl<'a, T: protobuf::Message> ToBytes<'a> for Protobuf<T> {
    type Bytes = Vec<u8>;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.0.write_to_bytes()?)
    }
}

#[cfg(feature = "protobuf")]
impl<T: Default + protobuf::Message> FromBytesOwned for Protobuf<T> {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Protobuf(T::parse_from_bytes(data)?))
    }
}

/// Raw does no conversion, it just copies the memory directly.
/// Note: This will only work for types that implement [bytemuck::Pod](https://docs.rs/bytemuck/latest/bytemuck/trait.Pod.html)
#[cfg(all(feature = "raw", target_endian = "little"))]
pub struct Raw<'a, T: bytemuck::Pod>(pub &'a T);

#[cfg(all(feature = "raw", target_endian = "little"))]
impl<'a, T: bytemuck::Pod> ToBytes<'a> for Raw<'a, T> {
    type Bytes = &'a [u8];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(bytemuck::bytes_of(self.0))
    }
}

#[cfg(all(feature = "raw", target_endian = "little"))]
impl<'a, T: bytemuck::Pod> FromBytes<'a> for Raw<'a, T> {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        let x = bytemuck::try_from_bytes(data).map_err(|x| Error::msg(x.to_string()))?;
        Ok(Raw(x))
    }
}

#[cfg(all(feature = "raw", target_endian = "big"))]
compile_error!("The raw feature is only supported on little endian targets");
