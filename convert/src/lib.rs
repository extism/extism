pub use anyhow::Error;

pub struct Json<T>(pub T);

pub trait ToBytes<'a> {
    type Bytes: AsRef<[u8]>;

    fn input(&'a self) -> Result<Self::Bytes, Error>;
}

impl<'a> ToBytes<'a> for Vec<u8> {
    type Bytes = &'a [u8];
    fn input(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self.as_slice())
    }
}

impl<'a> ToBytes<'a> for &'a [u8] {
    type Bytes = &'a [u8];
    fn input(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a> ToBytes<'a> for &'a str {
    type Bytes = &'a str;
    fn input(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a> ToBytes<'a> for String {
    type Bytes = &'a str;
    fn input(&'a self) -> Result<Self::Bytes, Error> {
        Ok(self)
    }
}

impl<'a, T: serde::Serialize> ToBytes<'a> for Json<T> {
    type Bytes = String;

    fn input(&'a self) -> Result<Self::Bytes, Error> {
        let enc = serde_json::to_string(&self.0)?;
        Ok(enc)
    }
}

pub trait FromBytes<'a>: Sized {
    fn output(data: &'a [u8]) -> Result<Self, Error>;
}

impl<'a> FromBytes<'a> for &'a [u8] {
    fn output(data: &'a [u8]) -> Result<Self, Error> {
        Ok(data)
    }
}

impl<'a> FromBytes<'a> for &'a str {
    fn output(data: &'a [u8]) -> Result<Self, Error> {
        Ok(std::str::from_utf8(data)?)
    }
}

impl<'a> FromBytes<'a> for Vec<u8> {
    fn output(data: &'a [u8]) -> Result<Self, Error> {
        Ok(data.to_vec())
    }
}

impl<'a> FromBytes<'a> for String {
    fn output(data: &'a [u8]) -> Result<Self, Error> {
        Ok(std::str::from_utf8(data)?.to_string())
    }
}

impl<'a, T: serde::Deserialize<'a>> FromBytes<'a> for Json<T> {
    fn output(data: &'a [u8]) -> Result<Self, Error> {
        let x = serde_json::from_slice(data)?;
        Ok(Json(x))
    }
}

#[cfg(feature = "msgpack")]
pub struct Msgpack<T>(pub T);

#[cfg(feature = "msgpack")]
impl<'a, T: serde::Deserialize<'a>> FromBytes<'a> for Msgpack<T> {
    fn output(data: &'a [u8]) -> Result<Self, Error> {
        let x = rmp_serde::from_slice(data)?;
        Ok(Msgpack(x))
    }
}

#[cfg(feature = "msgpack")]
impl<'a, T: serde::Serialize> ToBytes<'a> for Msgpack<T> {
    type Bytes = Vec<u8>;

    fn input(&'a self) -> Result<Self::Bytes, Error> {
        let enc = rmp_serde::to_vec(&self.0)?;
        Ok(enc)
    }
}
