import { Plugin } from './index.js';
import { readFileSync } from 'fs';

let wasm = readFileSync('../wasm/code.wasm');
let p = new Plugin(wasm);

if (!p.function_exists('count_vowels')) {
  console.log("no function 'count_vowels' in wasm");
  process.exit(1);
}

let buf = await p.call('count_vowels', process.argv[2] || 'this is a test');
console.log(JSON.parse(buf.toString())['count']);
