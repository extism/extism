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
wasm-strip extism-runtime.wasm
mv extism-runtime.wasm ../runtime/src/extism-runtime.wasm

