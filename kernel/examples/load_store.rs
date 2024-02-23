#![no_main]
#![no_std]
use extism_runtime_kernel::*;
use owi::*;

main!({
    reset();

    let x = u64();
    assume(x < 64);
    assume(x > 0);

    let m = alloc(x);
    assert(length(m) == x);
    for i in 0..x {
        store_u8(m + i, i as u8);
        assert(load_u8(m + i) == i as u8);
    }
    free(m);
});
