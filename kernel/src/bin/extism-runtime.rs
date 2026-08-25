#![cfg_attr(target_arch = "wasm32", no_main, no_std)]

#[cfg(target_arch = "wasm32")]
pub use extism_runtime_kernel::*;

#[cfg(all(target_arch = "wasm32", not(test)))]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    eprintln!(
        "extism-runtime is the wasm32 kernel; build with --target wasm32-unknown-unknown (see build.sh)"
    );
}
