#![no_main]
#![no_std]
use extism_runtime_kernel::*;
use owi::*;

main!({
    let x = u64();
    assume(x < i32::MAX as u64 / 2);
    assume(x > 0);

    alloc(1024);
    alloc(123);
    let n = alloc(x);
    assert(error_get() == 0);

    error_set(n);
    assert(error_get() == n);
    error_set(0);
    assert(error_get() == 0);
});
