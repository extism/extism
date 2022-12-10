const { withContext, Context, Function, ValType } = require('./dist/index.js');
const { readFileSync } = require('fs');

function f(inputs) {
  console.log(inputs[0].t);
  console.log("Hello from Javascript!");
  return inputs;
}

let testing_123 = new Function("testing_123", [ValType.I64], [ValType.I64], f);

let functions = [testing_123];

withContext(async function(context) {
  let wasm = readFileSync("../wasm/code.wasm");
  let p = context.plugin(wasm, wasi = true, functions = functions);

  if (!p.functionExists("count_vowels")) {
    console.log("no function 'count_vowels' in wasm");
    process.exit(1);
  }

  let buf = await p.call("count_vowels", process.argv[2] || "this is a test");
  console.log(JSON.parse(buf.toString())["count"]);
  p.free();
});

// or, use a context like this:
// let ctx = new Context();
// let wasm = readFileSync("../wasm/code.wasm");
// let p = ctx.plugin(wasm, wasi = true, functions = [testing_123]);
// ... where the context can be passed around to various functions etc.
