#![no_main]

use extism_pdk::*;

const VOWELS: &[char] = &['a', 'A', 'e', 'E', 'i', 'I', 'o', 'O', 'u', 'U'];

#[no_mangle]
unsafe fn count_vowels() -> i32 {
    let host = Host::new();
    let s = host.input_str();

    let mut count = 0;
    for ch in s.chars() {
        if VOWELS.contains(&ch) {
            count += 1;
        }
    }

    host.output(&format!(r#"{{"count": {count}}}"#));
    0
}
