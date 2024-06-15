#!/usr/bin/env bash

set -e

OUT_DIR=./target/wasm32-unknown-unknown/release/examples/
get_proof() {
   cp "$OUT_DIR/$1.wasm" "./proofs/$1.wasm"
}

cargo build --examples --release --target wasm32-unknown-unknown --no-default-features

mkdir -p proofs
get_proof alloc_length
get_proof load_store
get_proof reuse
get_proof error

for proof in $(ls proofs/*.wasm); do
  echo "Checking $proof"
  owi  conc "$proof" $@
  echo
  echo "---"
done
