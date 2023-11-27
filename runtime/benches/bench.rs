use criterion::{criterion_group, criterion_main, Criterion};
use extism::*;
use extism_convert::Json;

const COUNT_VOWELS: &[u8] = include_bytes!("../../wasm/code.wasm");
const REFLECT: &[u8] = include_bytes!("../../wasm/reflect.wasm");
const ECHO: &[u8] = include_bytes!("../../wasm/echo.wasm");
const CONSUME: &[u8] = include_bytes!("../../wasm/consume.wasm");

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
    let mut plugin = PluginBuilder::new(CONSUME)
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
    let mut plugin = PluginBuilder::new(ECHO)
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
    g.sample_size(500);
    g.noise_threshold(1.0);
    g.significance_level(0.2);
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
    ]
    .iter()
    .enumerate()
    {
        g.throughput(criterion::Throughput::Bytes(elements.len() as u64));
        g.bench_with_input(
            format!("reflect {} bytes", 10u32.pow(i as u32) * 65536),
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
    consume,
    echo,
    reflect,
    basic,
    create_plugin,
    count_vowels
);
criterion_main!(benches);
