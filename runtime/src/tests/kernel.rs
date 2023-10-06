use crate::*;

const KERNEL: &[u8] = include_bytes!("../extism-runtime.wasm");

fn extism_alloc<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, n: u64) -> u64 {
    let out_alloc = &mut [Val::I64(0)];
    instance
        .get_func(&mut store, "extism_alloc")
        .unwrap()
        .call(&mut store, &[Val::I64(n as i64)], out_alloc)
        .unwrap();
    out_alloc[0].unwrap_i64() as u64
}

fn extism_length<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) -> u64 {
    let out = &mut [Val::I64(0)];
    instance
        .get_func(&mut store, "extism_length")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], out)
        .unwrap();
    out[0].unwrap_i64() as u64
}

fn extism_free<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) {
    instance
        .get_func(&mut store, "extism_free")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], &mut [])
        .unwrap();
}

fn extism_error_set<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance, p: u64) {
    instance
        .get_func(&mut store, "extism_error_set")
        .unwrap()
        .call(&mut store, &[Val::I64(p as i64)], &mut [])
        .unwrap();
}

fn extism_error_get<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance) -> u64 {
    let out = &mut [Val::I64(0)];
    instance
        .get_func(&mut store, "extism_error_get")
        .unwrap()
        .call(&mut store, &[], out)
        .unwrap();

    out[0].unwrap_i64() as u64
}

fn extism_reset<T>(mut store: &mut wasmtime::Store<T>, instance: &mut Instance) {
    instance
        .get_func(&mut store, "extism_reset")
        .unwrap()
        .call(&mut store, &[], &mut [])
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
    assert!(p > 0);
    assert_eq!(extism_length(&mut store, instance, p), 1);
    extism_free(&mut store, instance, p);

    // 2 bytes
    let x = extism_alloc(&mut store, instance, 2);
    assert!(x > 0);
    assert!(x != p);
    assert_eq!(extism_length(&mut store, instance, x), 2);
    extism_free(&mut store, instance, x);

    // 64 bytes
    let p = extism_alloc(&mut store, instance, 64);
    assert!(p > 0);
    assert_eq!(extism_length(&mut store, instance, p), 64);
    extism_free(&mut store, instance, p);

    // 64 bytes, should re-use the last allocation
    let q = extism_alloc(&mut store, instance, 64);
    assert_eq!(p, q);
    assert_eq!(extism_length(&mut store, instance, q), 64);
    extism_free(&mut store, instance, q);

    // 512 bytes, test block re-use + splitting
    let p = extism_alloc(&mut store, instance, 512);
    assert_eq!(extism_length(&mut store, instance, p), 512);
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
    assert_eq!(extism_length(&mut store, instance, q), 128);
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
    assert_eq!(extism_length(&mut store, instance, q), 65536 + 1024);
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
