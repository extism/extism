import { Plugin } from "./index.js";
import { readFileSync } from "fs";

let wasm = readFileSync("../wasm/code.wasm");
let p = new Plugin(wasm);
let buf = p.call("count_vowels", process.argv[2] || "this is a test");
console.log(JSON.parse(buf.toString())['count']);