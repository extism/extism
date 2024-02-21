#![no_main]
#![no_std]

use extism_runtime_kernel::*;
use owi::*;

#[cfg(all(target_arch = "wasm32", not(test)))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

#[no_mangle]
pub unsafe fn _start() {
    reset();
    let n = alloc(1024);

    let x = u64();
    assume(x < i32::MAX as u64);
    assume(x > 0);

    let m = alloc(x);

    // 1. Length should equal `x` while active
    assert(length(m) == x);

    // 2. Length should equal `0` after free
    free(m); // Free the block
    assert(length(m) == 0);
    free(n);
}
