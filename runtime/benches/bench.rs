use criterion::{criterion_group, criterion_main, Criterion};
use extism::*;

const COUNT_VOWELS: &[u8] = include_bytes!("../../wasm/code.wasm");
const REFLECT: &[u8] = include_bytes!("../../wasm/reflect.wasm");

host_fn!(hello_world (a: String) -> String { Ok(a) });

pub fn basic(c: &mut Criterion) {
    let mut g = c.benchmark_group("basic");
    g.sample_size(300);
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
    g.sample_size(300);
    g.bench_function("create_plugin", |b| {
        b.iter(|| {
            let _plugin = PluginBuilder::new_with_module(COUNT_VOWELS)
                .with_wasi(true)
                .build()
                .unwrap();
        })
    });
}

pub fn count_vowels(c: &mut Criterion) {
    let mut g = c.benchmark_group("count_vowels");
    g.sample_size(500);
    let mut plugin = PluginBuilder::new_with_module(COUNT_VOWELS)
        .with_wasi(true)
        .build()
        .unwrap();
    let data = "a".repeat(4096);
    g.bench_function("count_vowels(4096)", |b| {
        b.iter(|| {
            assert_eq!(
                "{\"count\": 4096}",
                plugin.call::<_, &str>("count_vowels", &data).unwrap()
            );
        })
    });
}

pub fn reflect_1(c: &mut Criterion) {
    let mut g = c.benchmark_group("reflect_1");
    g.sample_size(500);
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    let mut plugin = PluginBuilder::new_with_module(REFLECT)
        .with_wasi(true)
        .with_function(
            "host_reflect",
            [ValType::I64],
            [ValType::I64],
            None,
            hello_world,
        )
        .build()
        .unwrap();
    let data = "a".repeat(65536);
    g.bench_function("reflect_1", |b| {
        b.iter(|| {
            assert_eq!(data, plugin.call::<_, &str>("reflect", &data).unwrap());
        })
    });
}

pub fn reflect_10(c: &mut Criterion) {
    let mut g = c.benchmark_group("reflect_10");
    g.sample_size(200);
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    let mut plugin = PluginBuilder::new_with_module(REFLECT)
        .with_wasi(true)
        .with_function(
            "host_reflect",
            [ValType::I64],
            [ValType::I64],
            None,
            hello_world,
        )
        .build()
        .unwrap();
    let data = "a".repeat(65536 * 10);
    g.bench_function("reflect_10", |b| {
        b.iter(|| {
            assert_eq!(data, plugin.call::<_, &str>("reflect", &data).unwrap());
        })
    });
}

pub fn reflect_100(c: &mut Criterion) {
    let mut g = c.benchmark_group("reflect_100");
    g.sample_size(50);
    g.noise_threshold(1.0);
    g.significance_level(0.2);
    let mut plugin = PluginBuilder::new_with_module(REFLECT)
        .with_wasi(true)
        .with_function(
            "host_reflect",
            [ValType::I64],
            [ValType::I64],
            None,
            hello_world,
        )
        .build()
        .unwrap();
    let data = "a".repeat(65536 * 100);
    g.bench_function("reflect_100", |b| {
        b.iter(|| {
            assert_eq!(data, plugin.call::<_, &str>("reflect", &data).unwrap());
        })
    });
}

criterion_group!(
    benches,
    basic,
    create_plugin,
    count_vowels,
    reflect_1,
    reflect_10,
    reflect_100
);
criterion_main!(benches);
