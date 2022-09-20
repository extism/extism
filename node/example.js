import { withContext } from './index.js';
import { readFileSync } from 'fs';

withContext(async function(context) {
  let wasm = readFileSync('../wasm/code.wasm');
  let p = context.plugin(wasm);

  if (!p.functionExists('count_vowels')) {
    console.log("no function 'count_vowels' in wasm");
    process.exit(1);
  }

  let buf = await p.call('count_vowels', process.argv[2] || 'this is a test');
  console.log(JSON.parse(buf.toString())['count']);
  p.free();
});
