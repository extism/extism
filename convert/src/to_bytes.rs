use crate::*;

pub use extism_convert_macros::ToBytes;

/// `ToBytes` is used to define how a type should be encoded when working with
/// Extism memory. It is used for plugin input and host function output.
///
/// `ToBytes` can be derived by delegating encoding to generic type implementing
/// `ToBytes`, e.g., [`Json`], [`Msgpack`].
///
/// ```
/// use extism_convert::{Json, ToBytes};
/// use serde::Serialize;
///
/// #[derive(ToBytes, Serialize)]
/// #[encoding(Json)]
/// struct Struct {
///     hello: String,
/// }
///
/// assert_eq!(Struct { hello: "hi".into() }.to_bytes()?, br#"{"hello":"hi"}"#);
/// # Ok::<(), extism_convert::Error>(())
/// ```
///
/// But custom types can also be used, as long as they are new-types with a single
/// generic argument, i.e., `Type<T>(T)`, that implement `ToBytes` for the struct.
///
/// ```
/// use extism_convert::{Error, ToBytes};
///
/// // Custom serialization using `ToString`
/// struct StringEnc<T>(T);
/// impl<T: ToString> ToBytes<'_> for StringEnc<&T> {
///     type Bytes = String;
///     
///     fn to_bytes(&self) -> Result<String, Error> {
///         Ok(self.0.to_string())
///     }
/// }
///
/// #[derive(ToBytes)]
/// #[encoding(StringEnc)]
/// struct Struct {
///     hello: String,
/// }
///
/// impl ToString for Struct {
///    fn to_string(&self) -> String {
///        self.hello.clone()     
///    }
/// }
///
/// assert_eq!(Struct { hello: "hi".into() }.to_bytes()?, b"hi");
/// # Ok::<(), Error>(())
/// ```
pub trait ToBytes<'a> {
    /// A configurable byte slice representation, allows any type that implements `AsRef<[u8]>`
    type Bytes: AsRef<[u8]>;

    /// `to_bytes` converts a value into `Self::Bytes`
    fn to_bytes(&self) -> Result<Self::Bytes, Error>;
}

impl ToBytes<'_> for () {
    type Bytes = [u8; 0];
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok([])
    }
}

impl ToBytes<'_> for Vec<u8> {
    type Bytes = Vec<u8>;
    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.clone())
    }
}

impl ToBytes<'_> for String {
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

impl ToBytes<'_> for f64 {
    type Bytes = [u8; 8];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl ToBytes<'_> for f32 {
    type Bytes = [u8; 4];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl ToBytes<'_> for i64 {
    type Bytes = [u8; 8];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl ToBytes<'_> for i32 {
    type Bytes = [u8; 4];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl ToBytes<'_> for u64 {
    type Bytes = [u8; 8];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl ToBytes<'_> for u32 {
    type Bytes = [u8; 4];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok(self.to_le_bytes())
    }
}

impl ToBytes<'_> for bool {
    type Bytes = [u8; 1];

    fn to_bytes(&self) -> Result<Self::Bytes, Error> {
        Ok([*self as u8])
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

#[test]
fn test() {
    use extism_convert::{Json, ToBytes};
    use serde::Serialize;

    #[derive(ToBytes, Serialize)]
    #[encoding(Json)]
    struct Struct {
        hello: String,
    }
}
