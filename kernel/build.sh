#!/usr/bin/env bash

export CARGO_FLAGS=""

while getopts d flag
do
    case "${flag}" in
        d)
            echo "Disabled bounds-checking";
            export CARGO_FLAGS="--no-default-features";;
        *)
            echo "usage $0 [-d]"
            echo "\t-d: build with bounds checking disabled"
            exit 1
    esac
done

cargo build --package extism-runtime-kernel --bin extism-runtime --release --target wasm32-unknown-unknown $CARGO_FLAGS
cp target/wasm32-unknown-unknown/release/extism-runtime.wasm .

wasm-tools parse extism-context.wat -o extism-context.wasm
wasm-merge --enable-reference-types ./extism-runtime.wasm runtime extism-context.wasm context -o ../runtime/src/extism-runtime.wasm
rm extism-context.wasm
rm extism-runtime.wasm
wasm-strip ../runtime/src/extism-runtime.wasm
