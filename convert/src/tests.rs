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
