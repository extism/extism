import {
  withContext,
  Context,
  HostFunction,
  ValType,
} from "./dist/index.js";
import { readFileSync } from "https://deno.land/std@0.120.0/node/module.ts";

function f(currentPlugin, inputs, outputs, userData) {
  console.log(currentPlugin.inputString(inputs[0]));
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
