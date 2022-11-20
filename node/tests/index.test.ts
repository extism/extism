import * as extism from "../src/index";
import { readFileSync } from "fs";
import { join } from "path";

function manifest(): extism.Manifest {
  return {
    wasm: [{ path: join(__dirname, "/code.wasm") }],
  };
}

function wasmBuffer(): Buffer {
  return readFileSync(join(__dirname, "/code.wasm"));
}

describe("test extism", () => {
  test("can create new context", () => {
    let ctx = new extism.Context();
    expect(ctx).toBeTruthy();
    ctx.free();
  });

  test("can create and call a plugin", async () => {
    await extism.withContext(async (ctx: extism.Context) => {
      const plugin = ctx.plugin(manifest());
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
  });

  test("can free a plugin", async () => {
    await extism.withContext(async (ctx: extism.Context) => {
      const plugin = ctx.plugin(manifest());
      let output = await plugin.call("count_vowels", "this is a test");
      plugin.free();
      await expect(() =>
        plugin.call("count_vowels", "this is a test")
      ).rejects.toMatch(/Plugin error/);
    });
  });

  test("can update the manifest", async () => {
    await extism.withContext(async (ctx: extism.Context) => {
      const plugin = ctx.plugin(manifest());
      let output = await plugin.call("count_vowels", "this is a test");
      let result = JSON.parse(output.toString());
      expect(result["count"]).toBe(4);
      // let's update the plugin with a manifest of raw module bytes
      plugin.update(wasmBuffer());
      // can still call it
      output = await plugin.call("count_vowels", "this is a test");
      result = JSON.parse(output.toString());
      expect(result["count"]).toBe(4);
    });
  });

  test("can detect if function exists or not", async () => {
    await extism.withContext(async (ctx: extism.Context) => {
      const plugin = ctx.plugin(manifest());
      expect(plugin.functionExists("count_vowels")).toBe(true);
      expect(plugin.functionExists("i_dont_extist")).toBe(false);
    });
  });

  test("withContext returns results", async () => {
    const count = await extism.withContext(
      async (ctx: extism.Context): Promise<number> => {
        const plugin = ctx.plugin(manifest());
        let output = await plugin.call("count_vowels", "this is a test");
        let result = JSON.parse(output.toString());
        return result["count"];
      }
    );
    expect(count).toBe(4);
  });

  test("errors when function is not known", async () => {
    await extism.withContext(async (ctx: extism.Context) => {
      const plugin = ctx.plugin(manifest());
      await expect(() =>
        plugin.call("i_dont_exist", "example-input")
      ).rejects.toMatch(/Plugin error/);
    });
  });

  test("can result context", async () => {
    await extism.withContext(async (ctx: extism.Context) => {
      const plugin = ctx.plugin(manifest());
      await plugin.call("count_vowels", "this is a test");
      ctx.reset();
      await expect(() =>
        plugin.call("i_dont_exist", "example-input")
      ).rejects.toMatch(/Plugin error/);
    });
  });
});
