# extism-convert

The [extism-convert](https://crates.io/crates/extism-convert) crate is used by the [Rust SDK](https://crates.io/crates/extism) and [Rust PDK](https://crates.io/crates/extism-pdk) to provide a shared interface for
encoding and decoding values that can be passed to Extism function calls.

A set of types (Json, Msgpack, Protobuf) that can be used to specify a serde encoding are also provided. These are
similar to [axum extractors](https://docs.rs/axum/latest/axum/extract/index.html#intro) - they are
implemented as a tuple struct with a single field that is meant to be extracted using pattern matching.

## Documentation

See [extism-convert on docs.rs](https://docs.rs/extism-convert/latest/extism_convert/) for in-depth documentation.
