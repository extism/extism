use extism_convert_macros::ToBytes;

#[derive(ToBytes)]
struct MissingEncoding;

#[derive(ToBytes)]
#[encoding]
struct EmptyAttr;

#[derive(ToBytes)]
#[encoding = "string"]
struct EqNoParen;

#[derive(ToBytes)]
#[encoding(something, else)]
struct NotAPath;

#[derive(ToBytes)]
#[encoding(Multiple)]
#[encoding(Encodings)]
struct MultipleEncodings;

fn main() {}
