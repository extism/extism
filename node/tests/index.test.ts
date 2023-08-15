import * as extism from "../src/index";
import { readFileSync } from "fs";
import { join } from "path";

function manifest(functions: boolean = false): extism.Manifest {
  return {
    wasm: [
      {
        path: join(
          __dirname,
          functions
            ? "/../../wasm/code-functions.wasm"
            : "/../../wasm/code.wasm",
        ),
      },
    ],
  };
}

function wasmBuffer(): Buffer {
  return readFileSync(join(__dirname, "/../../wasm/code.wasm"));
}

describe("test extism", () => {
  test("can create and call a plugin", async () => {
    const plugin = new extism.Plugin(manifest());
    let output = await plugin.call("count_vowels", "this is a test");
    let result = JSON.parse(output.toString());
    expect(result["count"]).toBe(4);
    output = await plugin.call("count_vowels", "this is a test again");
    result = JSON.parse(output.toString());
    expect(result["count"]).toBe(7);
    output = await plugin.call("count_vowels", "this is a test thrice");
    result = JSON.parse(output.toString());
    expect(result["count"]).toBe(6);
    output = await plugin.call("count_vowels", "🌎hello🌎world🌎");
    result = JSON.parse(output.toString());
    expect(result["count"]).toBe(3);
  });

  // test("can free a plugin", async () => {
  //   const plugin = new extism.Plugin(manifest());
  //   let output = await plugin.call("count_vowels", "this is a test");
  //   plugin.free();
  //   await expect(() => plugin.call("count_vowels", "this is a test")).rejects
  //     .toMatch("Plugin already freed");
  // });

  // test("can detect if function exists or not", async () => {
  //   const plugin = new extism.Plugin(manifest());
  //   expect(plugin.functionExists("count_vowels")).toBe(true);
  //   expect(plugin.functionExists("i_dont_extist")).toBe(false);
  // });

  // test("errors when function is not known", async () => {
  //   const plugin = new extism.Plugin(manifest());
  //   await expect(() => plugin.call("i_dont_exist", "example-input")).rejects
  //     .toMatch(/Plugin error/);
  // });

  // test("host functions work", async () => {
  //   const plugin = new extism.Plugin(manifest(true), true, [
  //     new extism.HostFunction(
  //       "hello_world",
  //       [extism.ValType.I64],
  //       [extism.ValType.I64],
  //       (plugin: any, params: any, results: any, user_data: string) => {
  //         const offs = plugin.memoryAlloc(user_data.length);
  //         const mem = plugin.memory(offs);
  //         mem.write(user_data);
  //         results[0].v.i64 = offs;
  //       },
  //       "test",
  //     ),
  //   ]);

  //   const res = await plugin.call("count_vowels", "aaa");

  //   expect(res.toString()).toBe("test");
  // });
});
