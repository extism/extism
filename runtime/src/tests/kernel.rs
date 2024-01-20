use crate::*;
use quickcheck::*;

const KERNEL: &[u8] = include_bytes!("../extism-runtime.wasm");

fn extism_alloc<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, n: u64) -> u64 {
    let out_alloc = &mut [Val::I64(0)];
    instance
        .get_func(&mut store, "alloc")
        .unwrap()
        .call(&mut store, &[Val::I64(n as i64)], out_alloc)
        .unwrap();
    out_alloc[0].unwrap_i64() as u64
}

fn extism_length<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) -> u64 {
    let out = &mut [Val::I64(0)];
    instance
        .get_func(&mut store, "length")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], out)
        .unwrap();
    out[0].unwrap_i64() as u64
}

fn extism_length_unsafe<T>(
    mut store: &mut wasmtime::Store<T>,
    instance: &mut Instance,
    p: u64,
) -> u64 {
    let out = &mut [Val::I64(0)];
    instance
        .get_func(&mut store, "length_unsafe")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], out)
        .unwrap();
    out[0].unwrap_i64() as u64
}

fn extism_load_u8<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) -> u8 {
    let out = &mut [Val::I32(0)];
    instance
        .get_func(&mut store, "load_u8")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], out)
        .unwrap();
    out[0].unwrap_i32() as u8
}

fn extism_load_u64<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) -> u64 {
    let out = &mut [Val::I32(0)];
    instance
        .get_func(&mut store, "load_u64")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], out)
        .unwrap();
    out[0].unwrap_i64() as u64
}

fn extism_input_load_u8<T>(
    mut store: &mut wasmtime::Store<T>,
    instance: &mut Instance,
    p: u64,
) -> u8 {
    let out = &mut [Val::I32(0)];
    instance
        .get_func(&mut store, "input_load_u8")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], out)
        .unwrap();
    out[0].unwrap_i32() as u8
}

fn extism_input_load_u64<T>(
    mut store: &mut wasmtime::Store<T>,
    instance: &mut Instance,
    p: u64,
) -> u64 {
    let out = &mut [Val::I32(0)];
    instance
        .get_func(&mut store, "input_load_u64")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], out)
        .unwrap();
    out[0].unwrap_i64() as u64
}

fn extism_store_u8<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64, x: u8) {
    instance
        .get_func(&mut store, "store_u8")
        .unwrap()
        .call(
            &mut store,
            &[Val::I64(p as i64), Val::I32(x as i32)],
            &mut [],
        )
        .unwrap();
}

fn extism_store_u64<T>(
    mut store: &mut wasmtime::Store<T>,
    instance: &mut Instance,
    p: u64,
    x: u64,
) {
    instance
        .get_func(&mut store, "store_u64")
        .unwrap()
        .call(
            &mut store,
            &[Val::I64(p as i64), Val::I64(x as i64)],
            &mut [],
        )
        .unwrap();
}

fn extism_free<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) {
    instance
        .get_func(&mut store, "free")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], &mut [])
        .unwrap();
}

fn extism_error_set<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) {
    instance
        .get_func(&mut store, "error_set")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], &mut [])
        .unwrap();
}

fn extism_error_get<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance) -> u64 {
    let out = &mut [Val::I64(0)];
    instance
        .get_func(&mut store, "error_get")
        .unwrap()
        .call(&mut store, &[], out)
        .unwrap();

    out[0].unwrap_i64() as u64
}

fn extism_reset<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance) {
    instance
        .get_func(&mut store, "reset")
        .unwrap()
        .call(&mut store, &[], &mut [])
        .unwrap();
}

fn extism_input_set<T>(
    mut store: &mut wasmtime::Store<T>,
    instance: &mut Instance,
    p: u64,
    l: u64,
) {
    instance
        .get_func(&mut store, "input_set")
        .unwrap()
        .call(
            &mut store,
            &[Val::I64(p as i64), Val::I64(l as i64)],
            &mut [],
        )
        .unwrap();
}

fn init_kernel_test() -> (Store<()>, Instance) {
    let config = wasmtime::Config::new();
    let engine = wasmtime::Engine::new(&config).unwrap();
    let mut store = wasmtime::Store::new(&engine, ());
    let module = wasmtime::Module::new(&engine, KERNEL).unwrap();
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).unwrap();
    (store, instance)
}

#[test]
fn test_kernel_allocations() {
    let (mut store, mut instance) = init_kernel_test();
    let instance = &mut instance;

    // Test allocations
    assert_eq!(extism_alloc(&mut store, instance, 0), 0);

    // 1 byte
    let p = extism_alloc(&mut store, instance, 1);
    let first_alloc = p;
    assert!(p > 0);
    assert_eq!(extism_length(&mut store, instance, p), 1);
    assert_eq!(extism_length_unsafe(&mut store, instance, p), 1);
    extism_free(&mut store, instance, p);

    // 2 bytes
    let x = extism_alloc(&mut store, instance, 2);
    assert!(x > 0);
    assert!(x != p);
    assert_eq!(extism_length(&mut store, instance, x), 2);
    assert_eq!(extism_length_unsafe(&mut store, instance, x), 2);
    extism_free(&mut store, instance, x);

    for i in 0..64 {
        let p = extism_alloc(&mut store, instance, 64 - i);
        assert!(p > 0);
        assert_eq!(extism_length(&mut store, instance, p), 64 - i);
        assert_eq!(extism_length_unsafe(&mut store, instance, p), 64 - i);
        extism_free(&mut store, instance, p);

        // should re-use the last allocation
        let q = extism_alloc(&mut store, instance, 64 - i);
        assert_eq!(p, q);
        assert_eq!(extism_length(&mut store, instance, q), 64 - i);
        assert_eq!(extism_length_unsafe(&mut store, instance, q), 64 - i);
        extism_free(&mut store, instance, q);
    }

    // 512 bytes, test block re-use + splitting
    let p = extism_alloc(&mut store, instance, 512);
    assert_eq!(extism_length(&mut store, instance, p), 512);
    assert_eq!(extism_length(&mut store, instance, p + 1), 0);
    assert_eq!(extism_length(&mut store, instance, p + 2), 0);
    assert_eq!(extism_length(&mut store, instance, p + 3), 0);
    assert_eq!(extism_length(&mut store, instance, p + 4), 0);
    extism_free(&mut store, instance, p);

    // 128 bytes, should be split off the 512 byte block
    let q = extism_alloc(&mut store, instance, 128);
    assert!(p <= q && q < p + 512);
    assert_eq!(extism_length(&mut store, instance, q), 128);
    extism_free(&mut store, instance, q);

    // 128 bytes, same as above
    let r = extism_alloc(&mut store, instance, 128);
    assert!(p <= r && r < p + 512);
    assert!(r > p);
    assert_eq!(extism_length(&mut store, instance, r), 128);
    extism_free(&mut store, instance, q);

    // 100 pages
    let p = extism_alloc(&mut store, instance, 6553600);
    assert!(p > 0);
    assert_eq!(extism_length(&mut store, instance, p), 6553600);
    extism_free(&mut store, instance, p);

    // One more page
    let p = extism_alloc(&mut store, instance, 65536);
    assert!(p > 0);
    assert_eq!(extism_length(&mut store, instance, p), 65536);

    // A little more than a page
    let p = extism_alloc(&mut store, instance, 65536 + 1024);
    assert!(p > 0);
    assert_eq!(extism_length(&mut store, instance, p), 65536 + 1024);
    extism_free(&mut store, instance, p);

    // Reset/sanity check
    extism_reset(&mut store, instance);
    let q = extism_alloc(&mut store, instance, 65536 + 1024);
    assert!(q < p);
    assert_eq!(first_alloc, q);
    assert_eq!(extism_length(&mut store, instance, q), 65536 + 1024);
    // Old pointer shouldn't return a valid length
    assert_eq!(extism_length(&mut store, instance, p), 0);
    extism_free(&mut store, instance, q);
}

#[test]
fn test_kernel_error() {
    let (mut store, mut instance) = init_kernel_test();
    let instance = &mut instance;

    let p = extism_alloc(&mut store, instance, 512);
    extism_error_set(&mut store, instance, p);
    assert_eq!(extism_error_get(&mut store, instance), p);
}

#[test]
fn test_load_store() {
    let (mut store, mut instance) = init_kernel_test();
    let instance = &mut instance;

    let p = extism_alloc(&mut store, instance, 8);
    extism_store_u64(&mut store, instance, p, 999);
    assert_eq!(extism_load_u64(&mut store, instance, p), 999);

    let mut buf = [0u8; 8];

    for (i, b) in buf.iter_mut().enumerate() {
        *b = extism_load_u8(&mut store, instance, p + i as u64);
    }
    assert_eq!(u64::from_le_bytes(buf), 999);

    for i in 0..8 {
        extism_store_u8(&mut store, instance, p + i as u64, i);
    }
    assert_eq!(extism_load_u64(&mut store, instance, p), 0x0706050403020100);

    // Reading/writing way out bounds shouldn't do anything, but since reads/writes aren't tied to blocks
    // it's hard to make sure a an offset falls inside a valid block
    assert_eq!(extism_load_u64(&mut store, instance, 0xffffffffffff), 0);
    extism_store_u8(&mut store, instance, 0xffffffffffff, 0);
}

#[test]
fn test_load_input() {
    let (mut store, mut instance) = init_kernel_test();
    let instance = &mut instance;

    let p = extism_alloc(&mut store, instance, 123456);

    for i in 0..123456 {
        extism_store_u8(&mut store, instance, p + i, b'a');
    }
    extism_input_set(&mut store, instance, p, 123456);

    for i in 0..123456 {
        assert_eq!(extism_input_load_u8(&mut store, instance, i), b'a');
    }

    // Out of bounds should return 0
    assert_eq!(extism_input_load_u64(&mut store, instance, 123457), 0);
}

#[test]
fn test_failed_quickcheck1() {
    let (mut store, mut instance) = init_kernel_test();
    let allocs = [
        20622, 23162, 58594, 32421, 25928, 44611, 26318, 24455, 5798, 60202, 42126, 64928, 57832,
        50888, 63256, 37562, 46334, 47985, 60836, 28132, 65535, 37800, 33150, 48768, 38457, 57249,
        5734, 58587, 26294, 26653, 24519, 1,
    ];

    extism_reset(&mut store, &mut instance);
    for a in allocs {
        println!("Alloc: {a}");
        let n = extism_alloc(&mut store, &mut instance, a);
        if n == 0 {
            continue;
        }
        assert_eq!(a, extism_length(&mut store, &mut instance, n));
    }
}

#[test]
fn test_failed_quickcheck2() {
    let (mut store, mut instance) = init_kernel_test();
    let allocs = [352054710, 1248853976, 2678441931, 14567928];

    extism_reset(&mut store, &mut instance);
    for a in allocs {
        println!("Alloc: {a}");
        let n = extism_alloc(&mut store, &mut instance, a);
        if n == 0 {
            continue;
        }
        assert_eq!(a, extism_length(&mut store, &mut instance, n));
    }
}

quickcheck! {
    fn check_alloc(amounts: Vec<u16>) -> bool {
        let (mut store, mut instance) = init_kernel_test();
        let instance = &mut instance;
        for a in amounts {
            let ptr = extism_alloc(&mut store, instance, a as u64);
            if ptr == 0 || ptr == u64::MAX {
                continue
            }
            if extism_length(&mut store, instance, ptr) != a as u64 {
                return false
            }
        }

        true
    }
}

quickcheck! {
    fn check_large_alloc(amounts: Vec<u32>) -> bool {
        let (mut store, mut instance) = init_kernel_test();
        let instance = &mut instance;
        for a in amounts {
            let ptr = extism_alloc(&mut store, instance, a as u64);
            if ptr == 0 {
                continue
            }
            let len = extism_length_unsafe(&mut store, instance, ptr);
            if len != a as u64 {
                return false
            }
        }

        true
    }
}

quickcheck! {
    fn check_alloc_with_frees(amounts: Vec<u16>) -> bool {
        let (mut store, mut instance) = init_kernel_test();
        let instance = &mut instance;
        let mut prev = 0;
        for a in amounts {
            let ptr = extism_alloc(&mut store, instance, a as u64);
            if ptr == 0 {
                continue
            }
            if extism_length(&mut store, instance, ptr) != a as u64  {
                return false
            }

            if a % 2 == 0 {
                extism_free(&mut store, instance, ptr);
            } else if a % 3 == 0 {
                extism_free(&mut store, instance, prev);
            }

            prev = ptr;
        }

        true
    }
}

quickcheck! {
    fn check_large_alloc_with_frees(amounts: Vec<u32>) -> bool {
        let (mut store, mut instance) = init_kernel_test();
        let instance = &mut instance;
        let mut prev = 0;
        for a in amounts {
            let ptr = extism_alloc(&mut store, instance, a as u64);
            if ptr == 0 || ptr == u64::MAX {
                continue
            }
            if extism_length(&mut store, instance, ptr) != a as u64 {
                return false
            }
            if a % 2 == 0 {
                extism_free(&mut store, instance, ptr);
            } else if a % 3 == 0 {
                extism_free(&mut store, instance, prev);
            }

            prev = ptr;

        }

        true
    }
}

quickcheck! {
    fn check_alloc_with_load_and_store(amounts: Vec<u16>) -> bool {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let (mut store, mut instance) = init_kernel_test();
        let instance = &mut instance;
        for a in amounts {
            let ptr = extism_alloc(&mut store, instance, a as u64);
            if ptr == 0 || ptr == u64::MAX {
                continue
            }
            if extism_length(&mut store, instance, ptr) != a as u64 {
                return false
            }

            for _ in 0..16 {
                let i = rng.gen_range(ptr..ptr+a as u64);
                extism_store_u8(&mut store, instance, i, i as u8);
                if extism_load_u8(&mut store, instance, i as u64) != i as u8 {
                    return false
                }
            }
        }

        true
    }
}
