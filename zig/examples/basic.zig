const std = @import("std");
const testing = std.testing;
const sdk = @import("extism");
const Context = sdk.Context;
const Plugin = sdk.Plugin;
const CurrentPlugin = sdk.CurrentPlugin;
const Function = sdk.Function;
const manifest = sdk.manifest;

export fn hello_world(plugin_ptr: ?*sdk.c.ExtismCurrentPlugin, inputs: [*c]const sdk.c.ExtismVal, n_inputs: u64, outputs: [*c]sdk.c.ExtismVal, n_outputs: u64, user_data: ?*anyopaque) callconv(.C) void {
    std.debug.print("Hello from Zig!\n", .{});
    const str_ud = @ptrCast([*:0]const u8, user_data orelse unreachable);
    std.debug.print("User data: {s}\n", .{str_ud});
    var input_slice = inputs[0..n_inputs];
    var output_slice = outputs[0..n_outputs];
    var curr_plugin = CurrentPlugin.getCurrentPlugin(plugin_ptr orelse unreachable);
    const input = curr_plugin.inputBytes(&input_slice[0]);
    std.debug.print("input: {s}\n", .{input});
    output_slice[0] = input_slice[0];
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    _ = sdk.setLogFile("extism.log", .Debug);
    var context = Context.init();
    defer context.deinit();

    const wasmfile_manifest = manifest.WasmFile{ .path = "../wasm/code-functions.wasm" };
    const man = .{ .wasm = &[_]manifest.Wasm{.{ .wasm_file = wasmfile_manifest }} };
    var f = Function.init(
        "hello_world",
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &hello_world,
        @constCast(@ptrCast(*const anyopaque, "user data")),
    );
    defer f.deinit();
    var my_plugin = try Plugin.initFromManifest(allocator, &context, man, &[_]Function{f}, true);
    // var my_plugin = try Plugin.init(allocator, &context, wasm, &[_]Function{f}, false);
    defer my_plugin.deinit();

    var config = std.StringHashMap([]const u8).init(allocator);
    defer config.deinit();
    try config.put("thing", "this is a really important thing");
    try my_plugin.setConfig(allocator, config);

    const input = "aeiouAEIOU____________________________________&smtms_y?" ** 1182;
    if (my_plugin.call("count_vowels", input)) |data| {
        std.debug.print("plugin output: {s}\n", .{data});
    } else |err| switch (err) {
        error.PluginCallFailed => {
            std.debug.print("plugin returned error: {s}\n", .{my_plugin.error_info.?});
        },
    }
    std.debug.print("extism version: {s}\n", .{sdk.extismVersion()});
    std.debug.print("has count_vowels: {}\n", .{my_plugin.hasFunction("count_vowels")});
}
