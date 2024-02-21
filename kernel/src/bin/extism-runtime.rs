#![no_std]
#![no_main]

pub use extism_runtime_kernel::*;

#[cfg(all(target_arch = "wasm32", not(test)))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

#[cfg(feature = "proof")]
mod proofs {
    use super::*;
    use owi::*;

    // Ensures that calling `length(x)=0` when `x` is within the bounds of
    // the global `MemoryRoot` memory section
    pub unsafe fn memory_root_length_0() {
        reset();
        let x = u64();
        assume(x < core::mem::size_of::<MemoryRoot>() as u64);
        let a = length(x as u64);
        assert(a == 0);
    }

    // Verifies that `length(alloc(x)) == x` while active, and `0` after
    // being freed.
    pub unsafe fn length_0_after_free() {
        reset();
        let x = u64();
        assume(x < i32::MAX as u64);
        let m = alloc(x); // Allocate a block
        assert(length(m) == x); // Length should equal `x`
        free(m); // Free the block
        assert(length(m) == 0); // Length should equal `0`
    }
}

#[no_mangle]
#[cfg(feature = "proof")]
pub unsafe fn _start() {
    proofs::memory_root_length_0();
    proofs::length_0_after_free();
}
