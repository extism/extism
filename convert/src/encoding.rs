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
#[cfg(feature = "protobuf")]
#[derive(Debug)]
pub struct Protobuf<T: prost::Message>(pub T);

#[cfg(feature = "protobuf")]
impl<T: prost::Message> From<T> for Protobuf<T> {
    fn from(data: T) -> Self {
        Self(data)
    }
}

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

/// Raw does no conversion, it just copies the memory directly.
/// Note: This should be used with caution and will not work with any Rust types that use pointers internally or
// implement `Drop` - it should primarily be used with `repr(C)` structs that embed numeric fields.
#[cfg(feature = "raw")]
pub struct Raw<'a, T> {
    ptr: *const T,
    _t: std::marker::PhantomData<&'a ()>,
}

#[cfg(feature = "raw")]
impl<'a, T> Raw<'a, T> {
    /// Create a new `Raw` handle from a reference.
    ///
    /// # Safety
    /// This is unsafe because there is no way to statically guarantee that `T` can be copied in an out of memory, as a user
    /// it is your job to ensure this is only used on structs that don't contain any pointers.
    pub unsafe fn new(data: &'a T) -> Self {
        Self {
            ptr: data as *const T,
            _t: Default::default(),
        }
    }

    /// Get a reference from the `Raw` handle.
    ///
    /// # Safety
    /// There is no guarantee that the resulting value was created from an actual `T`, so even if it was created from the corect
    /// number of bytes, the layout of the resulting value might not be correct.
    pub unsafe fn get(&self) -> &T {
        &*self.ptr
    }
}

#[cfg(feature = "raw")]
impl<'a, T> ToBytes<'a> for Raw<'a, T> {
    type Bytes = Vec<u8>;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        let mem = self.ptr as *const u8;
        let slice = unsafe { std::slice::from_raw_parts(mem, std::mem::size_of::<T>()) };
        Ok(slice.to_vec())
    }
}

#[cfg(feature = "raw")]
impl<'a, T> FromBytes<'a> for Raw<'a, T> {
    fn from_bytes(data: &[u8]) -> Result<Self, Error> {
        if data.len() != std::mem::size_of::<T>() {
            let name = std::any::type_name::<T>();
            anyhow::bail!("invalid memory size for Raw type: {}", name);
        }
        Ok(Raw {
            ptr: data.as_ptr() as *const T,
            _t: Default::default(),
        })
    }
}

#[test]
#[cfg(feature = "raw")]
fn test_raw() {
    #[derive(Debug, Clone, PartialEq)]
    struct TestRaw {
        a: i32,
        b: f64,
        c: bool,
    }
    let x = TestRaw {
        a: 123,
        b: 45678.91011,
        c: true,
    };
    let raw = unsafe { Raw::new(&x).to_bytes().unwrap() };
    let y = Raw::from_bytes(&raw).unwrap();
    assert_eq!(&x, unsafe { y.get() });

    let y: Result<Raw<[u8; std::mem::size_of::<TestRaw>()]>, Error> = Raw::from_bytes(&raw);
    assert!(y.is_ok());

    let y: Result<Raw<String>, Error> = Raw::from_bytes(&raw);
    assert!(y.is_err());
}
