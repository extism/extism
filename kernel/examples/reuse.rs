#![no_main]
#![no_std]
use extism_runtime_kernel::*;
use owi::*;

main!({
    reset();

    let x = u64();
    assume(x < i32::MAX as u64);
    assume(x > 0);

    let mut tmp = 0;

    for _ in 0..10 {
        let m = alloc(x);
        if tmp == 0 {
            tmp = m;
        } else {
            // Check that the existing block is being re-used
            assert(m == tmp);
        }

        assert(length(m) == x);
        free(m); // Free the block
    }

    // let y = u64();
    // assume(y == x + 1);
    // let n = alloc(y);
    // assert(n > tmp);

    let z = u64();
    assume(z <= x);
    assume(x - z < 32);
    assume(z > 0);
    let p = alloc(z);
    assert(p == tmp);
});
