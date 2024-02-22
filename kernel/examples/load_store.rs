#![no_main]
#![no_std]
use extism_runtime_kernel::*;
use owi::*;

main!({
    reset();

    let x = u64();
    assume(x < i32::MAX as u64);
    assume(x > 0);

    let offs = u64();
    assume(offs <= x);
    assume(offs > 0);

    let m = alloc(x);
    for i in 0..m {
        store_u8(m + i, i as u8);
        assert(load_u8(m + i) == i as u8);
    }
    free(m);
});
