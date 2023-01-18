const {
  withContext,
  Context,
  HostFunction,
  ValType,
} = require("./dist/index.js");
const { readFileSync } = require("fs");

function f(currentPlugin, inputs, outputs, userData) {
  let mem = currentPlugin.memory(inputs[0].v.i64);
  console.log(mem.length);
  console.log(mem.toString());
  console.log("Hello from Javascript!");
  console.log(userData);
  outputs[0] = inputs[0];
}

let hello_world = new HostFunction(
  "hello_world",
  [ValType.I64],
  [ValType.I64],
  f,
  "Hello again!"
);

let functions = [hello_world];

withContext(async function (context) {
  let wasm = readFileSync("../wasm/code-functions.wasm");
  let p = context.plugin(wasm, true, functions);

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
// let p = ctx.plugin(wasm);
// ... where the context can be passed around to various functions etc.
