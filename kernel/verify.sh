#!/usr/bin/env bash

set -e

cargo build --release --target wasm32-unknown-unknown --features proof
wasm2wat ./target/wasm32-unknown-unknown/release/extism-runtime.wasm > ./extism-runtime.proof.wat
owi sym ./extism-runtime.proof.wat $@
