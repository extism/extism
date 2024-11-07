use criterion::{criterion_group, criterion_main, Criterion};
use extism::*;
use extism_convert::Json;

const COUNT_VOWELS: &[u8] = include_bytes!("../../wasm/code.wasm");
const REFLECT: &[u8] = include_bytes!("../../wasm/reflect.wasm");
const ECHO: &[u8] = include_bytes!("../../wasm/echo.wasm");
const CONSUME: &[u8] = include_bytes!("../../wasm/consume.wasm");
const ALLOCATIONS: &[u8] = include_bytes!("../../wasm/allocations.wasm");

host_fn!(hello_world (a: String) -> String { Ok(a) });

pub fn basic(c: &mut Criterion) {
    let mut g = c.benchmark_group("basic");
    g.measurement_time(std::time::Duration::from_secs(6));
    g.bench_function("basic", |b| {
        let data = "a".repeat(4096);
        b.iter(|| {
            let mut plugin = Plugin::new(COUNT_VOWELS, [], true).unwrap();
            let _: serde_json::Value = plugin.call("count_vowels", &data).unwrap();
        })
    });
}

pub fn create_plugin(c: &mut Criterion) {
    let mut g = c.benchmark_group("create");
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    g.bench_function("create_plugin", |b| {
        b.iter(|| {
            let _plugin = PluginBuilder::new(COUNT_VOWELS)
                .with_wasi(true)
                .build()
                .unwrap();
        })
    });
}

pub fn create_compiled(c: &mut Criterion) {
    let mut g = c.benchmark_group("create");
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    g.bench_function("create_compiled", |b| {
        b.iter(|| {
            let plugin = PluginBuilder::new(COUNT_VOWELS).with_wasi(true);
            let _compiled = CompiledPlugin::new(plugin).unwrap();
        })
    });
}

pub fn create_plugin_compiled(c: &mut Criterion) {
    let mut g = c.benchmark_group("create");
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    let plugin = PluginBuilder::new(COUNT_VOWELS).with_wasi(true);
    let compiled = CompiledPlugin::new(plugin).unwrap();
    g.bench_function("create_plugin_compiled", |b| {
        b.iter(|| {
            let _plugin = Plugin::new_from_compiled(&compiled).unwrap();
        })
    });
}

pub fn create_plugin_no_cache(c: &mut Criterion) {
    let mut g = c.benchmark_group("create");
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    g.bench_function("create_plugin_no_cache", |b| {
        b.iter(|| {
            let _plugin = PluginBuilder::new(COUNT_VOWELS)
                .with_cache_disabled()
                .with_wasi(true)
                .build()
                .unwrap();
        })
    });
}

#[derive(Debug, serde::Deserialize, PartialEq)]
struct Count {
    count: u32,
}
pub fn count_vowels(c: &mut Criterion) {
    let mut g = c.benchmark_group("count_vowels");
    g.sample_size(500);
    let mut plugin = PluginBuilder::new(COUNT_VOWELS)
        .with_wasi(true)
        .build()
        .unwrap();
    let data = "a".repeat(4096);
    g.bench_function("count_vowels(4096)", |b| {
        b.iter(|| {
            assert_eq!(
                Count { count: 4096 },
                plugin
                    .call::<_, Json<Count>>("count_vowels", &data)
                    .unwrap()
                    .0
            );
        })
    });
}

pub fn consume(c: &mut Criterion) {
    let mut g = c.benchmark_group("consume");
    g.sample_size(500);
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    let mut plugin = PluginBuilder::new(CONSUME).build().unwrap();

    for (i, elements) in [
        b"a".repeat(65536),
        b"a".repeat(65536 * 10),
        b"a".repeat(65536 * 100),
    ]
    .iter()
    .enumerate()
    {
        g.throughput(criterion::Throughput::Bytes(elements.len() as u64));
        g.bench_with_input(
            format!("consume {} bytes", 10u32.pow(i as u32) * 65536),
            elements,
            |b, elems| {
                b.iter(|| {
                    assert_eq!(b"", plugin.call::<_, &[u8]>("consume", &elems).unwrap());
                });
            },
        );
    }
}

pub fn echo(c: &mut Criterion) {
    let mut g = c.benchmark_group("echo");
    g.sample_size(500);
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    let mut plugin = PluginBuilder::new(ECHO).build().unwrap();

    for (i, elements) in [
        b"a".repeat(65536),
        b"a".repeat(65536 * 10),
        b"a".repeat(65536 * 100),
    ]
    .iter()
    .enumerate()
    {
        g.throughput(criterion::Throughput::Bytes(elements.len() as u64));
        g.bench_with_input(
            format!("echo {} bytes", 10u32.pow(i as u32) * 65536),
            elements,
            |b, elems| {
                b.iter(|| {
                    plugin.call::<_, &[u8]>("echo", &elems).unwrap();
                });
            },
        );
    }
}

pub fn reflect(c: &mut Criterion) {
    let mut g = c.benchmark_group("reflect");

    let mut plugin = PluginBuilder::new(REFLECT)
        .with_wasi(true)
        .with_function(
            "host_reflect",
            [PTR],
            [PTR],
            UserData::default(),
            hello_world,
        )
        .build()
        .unwrap();
    for (i, elements) in [
        b"a".repeat(65536),
        b"a".repeat(65536 * 10),
        b"a".repeat(65536 * 100),
        b"a".repeat(65536),
    ]
    .iter()
    .enumerate()
    {
        g.throughput(criterion::Throughput::Bytes(elements.len() as u64));
        g.bench_with_input(
            format!("{i}: reflect {} bytes", elements.len()),
            elements,
            |b, elems| {
                b.iter(|| {
                    assert_eq!(elems, plugin.call::<_, &[u8]>("reflect", &elems).unwrap());
                    // plugin.reset().unwrap();
                });
            },
        );
    }
}

pub fn allocations(c: &mut Criterion) {
    let mut g = c.benchmark_group("allocations");

    let mut plugin = PluginBuilder::new(ALLOCATIONS).build().unwrap();
    g.bench_function("allocations", |b| {
        b.iter(|| {
            plugin.call::<_, ()>("allocations", "").unwrap();
        })
    });
}

// This is an apples-to-apples comparison of a linked wasm "reflect" function to our host "reflect"
// function.
pub fn reflect_linked(c: &mut Criterion) {
    let mut g = c.benchmark_group("reflect");
    g.sample_size(500);
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    let manifest = Manifest::new([
        Wasm::Data {
            data: br#"(module
    (import "extism:host/env" "length" (func $length (param i64) (result i64)))
    (import "extism:host/env" "load_u64" (func $load_u64 (param i64) (result i64)))
    (import "extism:host/env" "load_u8" (func $load_u8 (param i64) (result i32)))
    (import "extism:host/env" "store_u64" (func $store_u64 (param i64 i64)))
    (import "extism:host/env" "store_u8" (func $store_u8 (param i64 i32)))

    (func (export "host_reflect") (param $extism_offset i64) (result i64)
        (local $len i64)
        (local $offset i64)
        (local $len64 i64)

        (local.set $offset (i64.const 0))
        (local.set $len (call $length (local.get $extism_offset)))

        (local.set $len64 (i64.shl (i64.shr_u (local.get $len) (i64.const 16)) (i64.const 16)))

        (loop $to_upper
            (call $store_u64
              (i64.add (local.get $extism_offset) (local.get $offset))
              (call $load_u64 (i64.add (local.get $extism_offset) (local.get $offset)))
            )

            (local.set $offset (i64.add (local.get $offset) (i64.const 8)))
            (br_if $to_upper (i64.lt_u (local.get $offset) (local.get $len64)))
        )

        (if (i64.ne (local.get $len64) (local.get $len)) (then
          (loop $to_upper
              (call $store_u8
                (i64.add (local.get $extism_offset) (local.get $offset))
                (call $load_u8 (i64.add (local.get $extism_offset) (local.get $offset)))
              )

              (local.set $offset (i64.add (i64.const 1) (local.get $offset)))
              (br_if $to_upper (i64.lt_u (local.get $offset) (local.get $len)))
          )
        ))
        local.get $extism_offset
    )
)"#
            .to_vec(),
            meta: WasmMetadata {
                name: Some("extism:host/user".to_string()),
                hash: None,
            },
        },
        Wasm::Data {
            data: REFLECT.to_vec(),
            meta: WasmMetadata {
                name: Some("main".to_string()),
                hash: None,
            },
        },
    ]);
    let mut plugin = PluginBuilder::new(manifest)
        .with_wasi(true)
        .build()
        .unwrap();

    for (i, elements) in [
        b"a".repeat(65536),
        b"a".repeat(65536 * 10),
        b"a".repeat(65536 * 100),
    ]
    .iter()
    .enumerate()
    {
        g.throughput(criterion::Throughput::Bytes(elements.len() as u64));
        g.bench_with_input(
            format!("reflect_linked {} bytes", 10u32.pow(i as u32) * 65536),
            elements,
            |b, elems| {
                b.iter(|| {
                    assert_eq!(elems, plugin.call::<_, &[u8]>("reflect", &elems).unwrap());
                });
            },
        );
    }
}

criterion_group!(
    benches,
    allocations,
    consume,
    echo,
    reflect,
    reflect_linked,
    basic,
    create_plugin,
    create_plugin_compiled,
    create_plugin_no_cache,
    create_compiled,
    count_vowels
);
criterion_main!(benches);
