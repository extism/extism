use crate::*;

/// `FromBytes` is used to define how a type should be decoded when working with
/// Extism memory. It is used for plugin output and host function input.
pub trait FromBytes<'a>: Sized {
    /// Decode a value from a slice of bytes
    fn from_bytes(data: &'a [u8]) -> Result<Self, Error>;
}

/// `FromBytesOwned` is similar to `FromBytes` but it doesn't borrow from the input slice.
/// `FromBytes` is automatically implemented for all types that implement `FromBytesOwned`
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
