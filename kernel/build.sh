#!/usr/bin/env bash

cargo build --release --target wasm32-unknown-unknown --package extism-runtime-kernel --bin extism-runtime
cp target/wasm32-unknown-unknown/release/extism-runtime.wasm .
wasm-strip extism-runtime.wasm || :
mv extism-runtime.wasm ../runtime/src/extism-runtime.wasm

