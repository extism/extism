//! This crate is used to generate `libextism` using `extism-runtime`

pub use extism::sdk::*;

#[cfg(test)]
#[test]
fn test_version() {
    let s = unsafe { std::ffi::CStr::from_ptr(extism_version()) };
    assert!(s.to_bytes() != b"0.0.0");
}
