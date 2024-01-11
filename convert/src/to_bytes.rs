use crate::*;

/// `ToBytes` is used to define how a type should be encoded when working with
/// Extism memory. It is used for plugin input and host function output.
pub trait ToBytes<'a> {
    /// A configurable byte slice representation, allows any type that implements `AsRef<[u8]>`
    type Bytes: AsRef<[u8]>;

    /// `to_bytes` converts a value into `Self::Bytes`
    fn to_bytes(&self) -> Result<Self::Bytes, Error>;
}

impl<'a> ToBytes<'a> for () {
    type Bytes = [u8; 0];
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok([])
    }
}

impl<'a> ToBytes<'a> for Vec<u8> {
    type Bytes = Vec<u8>;
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.clone())
    }
}

impl<'a> ToBytes<'a> for String {
    type Bytes = String;
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.clone())
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

impl<'a> ToBytes<'a> for i64 {
    type Bytes = [u8; 8];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl<'a> ToBytes<'a> for i32 {
    type Bytes = [u8; 4];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl<'a> ToBytes<'a> for u64 {
    type Bytes = [u8; 8];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl<'a> ToBytes<'a> for u32 {
    type Bytes = [u8; 4];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl<'a, T: ToBytes<'a>> ToBytes<'a> for &'a T {
    type Bytes = T::Bytes;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        <T as ToBytes>::to_bytes(self)
    }
}

impl<'a, T: ToBytes<'a>> ToBytes<'a> for Option<T> {
    type Bytes = Vec<u8>;

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        match self {
            Some(x) => x.to_bytes().map(|x| x.as_ref().to_vec()),
            None => Ok(vec![]),
        }
    }
}
