use crate::*;
use std::time::Duration;

fn run_thread(p: Pool, i: u64) -> std::thread::JoinHandle<()> {
    std::thread::spawn(move || {
        std::thread::sleep(Duration::from_millis(i));
        let s: String = p
            .get(Duration::from_secs(1))
            .unwrap()
            .unwrap()
            .call("count_vowels", "abc")
            .unwrap();
        println!("{s}");
    })
}

fn init(max_instances: usize) -> Pool {
    let data = include_bytes!("../../../wasm/code.wasm");
    let plugin_builder =
        extism::PluginBuilder::new(extism::Manifest::new([extism::Wasm::data(data)]))
            .with_wasi(true);
    PoolBuilder::new()
        .with_max_instances(max_instances)
        .build(move || plugin_builder.clone().build())
}

#[test]
fn test_threads() {
    for i in 1..=3 {
        let pool = init(i);
        let threads = vec![
            run_thread(pool.clone(), 1000),
            run_thread(pool.clone(), 1000),
            run_thread(pool.clone(), 1000),
            run_thread(pool.clone(), 1000),
            run_thread(pool.clone(), 1000),
            run_thread(pool.clone(), 1000),
            run_thread(pool.clone(), 500),
            run_thread(pool.clone(), 500),
            run_thread(pool.clone(), 500),
            run_thread(pool.clone(), 500),
            run_thread(pool.clone(), 500),
            run_thread(pool.clone(), 0),
        ];

        for t in threads {
            t.join().unwrap();
        }

        assert!(pool.count() <= i);
    }
}

#[test]
fn test_exists() -> Result<(), Error> {
    let pool = init(1);
    let timeout = Duration::from_secs(1);
    assert!(pool.function_exists("count_vowels", timeout)?);
    assert!(pool.function_exists("count_vowels", timeout)?);
    assert!(!pool.function_exists("not_existing", timeout)?);
    assert!(!pool.function_exists("not_existing", timeout)?);
    Ok(())
}
