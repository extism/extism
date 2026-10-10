use crate::*;

const WASM: &[u8] = include_bytes!("../../../wasm/code.wasm");

#[test]
fn memory_status_none_without_limiter() {
    let plugin = Plugin::new(WASM, [], true).unwrap();
    assert!(plugin.memory_status().is_none());
}

#[test]
fn memory_status_some_with_limiter() {
    let manifest =
        extism_manifest::Manifest::new([extism_manifest::Wasm::data(WASM)]).with_memory_max(2);
    let plugin = Plugin::new(&manifest, [], true).unwrap();
    let status = plugin.memory_status().expect("should be Some");
    assert_eq!(status.max_bytes, 2 * 65536);
    assert_eq!(status.bytes_left, status.max_bytes);
}

#[test]
fn memory_status_clone_copy_debug() {
    let status = MemoryStatus {
        bytes_left: 1024,
        max_bytes: 2048,
    };
    // Test Copy
    let copied = status;
    assert_eq!(copied.bytes_left, status.bytes_left);
    assert_eq!(copied.max_bytes, status.max_bytes);
    // Test Clone
    let cloned = status.clone();
    assert_eq!(cloned.bytes_left, status.bytes_left);
    assert_eq!(cloned.max_bytes, status.max_bytes);
    // Test Debug
    let debug_str = format!("{:?}", status);
    assert!(debug_str.contains("1024"));
    assert!(debug_str.contains("2048"));
}
