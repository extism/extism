#!/usr/bin/env bash

set -e

OUT_DIR=./target/wasm32-unknown-unknown/release/examples/
get_proof() {
  wasm2wat "$OUT_DIR/$1.wasm" > "./proofs/$1.wat"
}

cargo build --examples --release --target wasm32-unknown-unknown --no-default-features

mkdir -p proofs
# Collect proofs in wat format
get_proof alloc_length
get_proof load_store

for proof in $(ls proofs); do
  echo "Checking $proof"
  owi sym "proofs/$proof" $@
  echo
  echo "---"
done
