
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
