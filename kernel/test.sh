# install wasm-bindgen-cli to get wasm-bindgen-runner if it is not installed yet
which wasm-bindgen-test-runner 1>/dev/null || cargo install -f wasm-bindgen-cli

# run tests with the wasm-bindgen-runner
CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_RUNNER=wasm-bindgen-test-runner cargo test --release --target=wasm32-unknown-unknown
