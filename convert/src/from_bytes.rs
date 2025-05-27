use crate::*;

pub use extism_convert_macros::FromBytes;

/// `FromBytes` is used to define how a type should be decoded when working with
/// Extism memory. It is used for plugin output and host function input.
///
/// `FromBytes` can be derived by delegating encoding to generic type implementing
/// `FromBytes`, e.g., [`Json`], [`Msgpack`].
///
/// ```
/// use extism_convert::{Json, FromBytes};
/// use serde::Deserialize;
///
/// #[derive(FromBytes, Deserialize, PartialEq, Debug)]
/// #[encoding(Json)]
/// struct Struct {
///     hello: String,
/// }
///
/// assert_eq!(Struct::from_bytes(br#"{"hello":"hi"}"#)?, Struct { hello: "hi".into() });
/// # Ok::<(), extism_convert::Error>(())
/// ```
///
/// Custom encodings can also be used, through new-types with a single generic
/// argument, i.e., `Type<T>(T)`, that implement `FromBytesOwned` for the struct.
///
/// ```
/// use std::str::{self, FromStr};
/// use std::convert::Infallible;
/// use extism_convert::{Error, FromBytes, FromBytesOwned};
///
/// // Custom serialization using `FromStr`
/// struct StringEnc<T>(T);
/// impl<T: FromStr> FromBytesOwned for StringEnc<T> where Error: From<<T as FromStr>::Err> {
///     fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
///         Ok(Self(str::from_utf8(data)?.parse()?))
///     }
/// }
///
/// #[derive(FromBytes, PartialEq, Debug)]
/// #[encoding(StringEnc)]
/// struct Struct {
///     hello: String,
/// }
///
/// impl FromStr for Struct {
///     type Err = Infallible;
///     fn from_str(s: &str) -> Result<Self, Infallible> {
///         Ok(Self { hello: s.to_owned() })
///     }
/// }
///
/// assert_eq!(Struct::from_bytes(b"hi")?, Struct { hello: "hi".into() });
/// # Ok::<(), extism_convert::Error>(())
/// ```
pub trait FromBytes<'a>: Sized {
    /// Decode a value from a slice of bytes
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error>;
}

/// `FromBytesOwned` is similar to [`FromBytes`] but it doesn't borrow from the input slice.
/// [`FromBytes`] is automatically implemented for all types that implement `FromBytesOwned`.
///
/// `FromBytesOwned` can be derived through [`#[derive(FromBytes)]`](FromBytes).
pub trait FromBytesOwned: Sized {
    /// Decode a value from a slice of bytes, the resulting value should not borrow the input
    /// data.
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error>;
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

impl<'a, T: FromBytesOwned> FromBytes<'a> for T {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        T::from_bytes_owned(data)
    }
}

impl FromBytesOwned for Box<[u8]> {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(data.to_vec().into_boxed_slice())
    }
}

impl FromBytesOwned for Vec<u8> {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(data.to_vec())
    }
}

impl FromBytesOwned for String {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(std::str::from_utf8(data)?.to_string())
    }
}

impl FromBytesOwned for f64 {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

impl FromBytesOwned for f32 {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

impl FromBytesOwned for i64 {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

impl FromBytesOwned for i32 {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

impl FromBytesOwned for u64 {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

impl FromBytesOwned for u32 {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        Ok(Self::from_le_bytes(data.try_into()?))
    }
}

impl FromBytesOwned for bool {
    fn from_bytes_owned(data: &[u8]) -> Result<Self, Error> {
        if let Some(x) = data.first() {
            Ok(*x != 0)
        } else {
            Err(Error::msg("Expected one byte to read boolean value"))
        }
    }
}

impl FromBytesOwned for () {
    fn from_bytes_owned(_: &[u8]) -> Result<Self, Error> {
        Ok(())
    }
}

impl<'a, T: FromBytes<'a>> FromBytes<'a> for std::io::Cursor<T> {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        Ok(std::io::Cursor::new(T::from_bytes(data)?))
    }
}

impl<'a, T: FromBytes<'a>> FromBytes<'a> for Option<T> {
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error> {
        if data.is_empty() {
            return Ok(None);
        }

        T::from_bytes(data).map(Some)
    }
}
