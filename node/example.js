const {
  Plugin,
  HostFunction,
  ValType,
} = require("./dist/index.js");
const { readFileSync } = require("fs");

function f(currentPlugin, inputs, outputs, userData) {
  console.log(currentPlugin.inputString(inputs[0]));
  console.log("Hello from Javascript!");
  console.log(userData);
  outputs[0] = inputs[0];
}

const hello_world = new HostFunction(
  "hello_world",
  [ValType.I64],
  [ValType.I64],
  f,
  "Hello again!",
);

async function main() {
  const functions = [hello_world];

  const wasm = readFileSync("../wasm/code-functions.wasm");
  const p = new Plugin(wasm, true, functions);

  if (!p.functionExists("count_vowels")) {
    console.log("no function 'count_vowels' in wasm");
    process.exit(1);
  }

  const buf = await p.call("count_vowels", process.argv[2] || "this is a test");
  console.log(JSON.parse(buf.toString())["count"]);
  p.free();
}

main();

// or, use a context like this:
// let ctx = new Context();
// let wasm = readFileSync("../wasm/code.wasm");
// let p = ctx.plugin(wasm);
// ... where the context can be passed around to various functions etc.
