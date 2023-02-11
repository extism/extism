
# Test wasm files
## code.wasm
code.wasm is an example Rust wasm application that has an exposed method `count_vowels`.  It takes in a string and returns a json object of the form `{"count": 3}`.
## sleepMs.wasm 
sleepMs.wasm is a js wasm module that takes in a json array with a single numeric (eg. `[5000]`) and sleeps for that many milliseconds before returning.