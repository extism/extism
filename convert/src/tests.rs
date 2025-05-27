use crate::*;

#[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
struct Testing {
    a: String,
    b: i64,
    c: f32,
}

#[test]
fn roundtrip_json() {
    let x = Testing {
        a: "foobar".to_string(),
        b: 123,
        c: 456.7,
    };
    let bytes = Json(&x).to_bytes().unwrap();
    let Json(y): Json<Testing> = FromBytes::from_bytes(&bytes).unwrap();
    assert_eq!(x, y);
}

#[test]
#[cfg(feature = "msgpack")]
fn roundtrip_msgpack() {
    let x = Testing {
        a: "foobar".to_string(),
        b: 123,
        c: 456.7,
    };
    let bytes = Msgpack(&x).to_bytes().unwrap();
    let Msgpack(y): Msgpack<Testing> = FromBytes::from_bytes(&bytes).unwrap();
    assert_eq!(x, y);
}

#[test]
fn roundtrip_base64() {
    let bytes = Base64("this is a test").to_bytes().unwrap();
    let Base64(s): Base64<String> = FromBytes::from_bytes(bytes.as_bytes()).unwrap();
    assert_eq!(s, "this is a test");
}

#[test]
fn rountrip_option() {
    // `None` case
    let e0: Option<Json<Testing>> = FromBytes::from_bytes(&[]).unwrap();
    let b = e0.to_bytes().unwrap();
    let e1: Option<Json<Testing>> = FromBytes::from_bytes(&b).unwrap();
    assert!(e0.is_none());
    assert_eq!(e0.is_none(), e1.is_none());

    // `Some` case
    let x = Testing {
        a: "foobar".to_string(),
        b: 123,
        c: 456.7,
    };
    let bytes = Json(&x).to_bytes().unwrap();
    let y: Option<Json<Testing>> = FromBytes::from_bytes(&bytes).unwrap();
    let b = ToBytes::to_bytes(&y).unwrap();
    let z: Option<Json<Testing>> = FromBytes::from_bytes(&b).unwrap();
    assert_eq!(y.unwrap().0, z.unwrap().0);
}

#[test]
fn check_bool() {
    // `None` case
    let a = true.to_bytes().unwrap();
    let b = false.to_bytes().unwrap();
    assert_ne!(a, b);

    assert_eq!(a, [1]);
    assert_eq!(b, [0]);
}

#[cfg(all(feature = "raw", target_endian = "little"))]
mod raw_tests {
    use crate::*;

    #[test]
    fn test_raw() {
        #[derive(Debug, Clone, Copy, PartialEq)]
        struct TestRaw {
            a: i32,
            b: f64,
            c: bool,
        }
        unsafe impl bytemuck::Pod for TestRaw {}
        unsafe impl bytemuck::Zeroable for TestRaw {}
        let x = TestRaw {
            a: 123,
            b: 45678.91011,
            c: true,
        };
        let raw = Raw(&x).to_bytes().unwrap();
        let y = Raw::from_bytes(raw).unwrap();
        assert_eq!(&x, y.0);

        let y: Result<Raw<[u8; std::mem::size_of::<TestRaw>()]>, Error> = Raw::from_bytes(raw);
        assert!(y.is_ok());
    }
}
