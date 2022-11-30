const { withContext, Context } = require('./dist/index.js');
const { readFileSync } = require('fs');

withContext(async function (context) {
  let wasm = readFileSync("../wasm/code.wasm");
  let p = context.plugin(wasm);

  if (!p.functionExists("count_vowels")) {
    console.log("no function 'count_vowels' in wasm");
    process.exit(1);
  }

  let buf = await p.call("count_vowels", process.argv[2] || "this is a test");
  console.log(JSON.parse(buf.toString())["count"]);
  p.free();
});

// or, use a context like this:
let ctx = new Context();
let wasm = readFileSync("../wasm/code.wasm");
let p = ctx.plugin(wasm);
// ... where the context can be passed around to various functions etc.
