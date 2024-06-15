#![no_main]
#![no_std]
use extism_runtime_kernel::*;
use owi::*;

main!({
    let x = u64();
    let n = u64();
    let m = u64();
    assume(x > 0);
    assume(n > 0);
    assume(m > 0);

    alloc(n);
    alloc(m);
    let n = alloc(x);
    assert(error_get() == 0);

    error_set(n);
    assert(error_get() == n);
    error_set(0);
    assert(error_get() == 0);
});
