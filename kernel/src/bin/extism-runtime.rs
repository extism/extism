#![no_std]
#![no_main]

pub use extism_runtime_kernel::*;

#[cfg(all(target_arch = "wasm32", not(test)))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

#[no_mangle]
pub unsafe fn _start() {
    // Check that no length is valid when nothing has been allocated
    reset();
    let x = i64_symbol();
    let a = length(x as u64);
    if a != 0 {
        core::arch::wasm32::unreachable()
    }
}
