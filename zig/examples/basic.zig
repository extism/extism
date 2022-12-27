const std = @import("std");
const testing = std.testing;
const sdk = @import("extism-zig-sdk");
const Context = sdk.Context;
const Plugin = sdk.Plugin;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    // var dir = try std.fs.cwd().openDir("../wasm", .{});
    // const wasm_file = try dir.openFile("code.wasm", .{});
    const wasm_file = try std.fs.cwd().openFile("test.wasm", .{});
    const wasm = try wasm_file.readToEndAlloc(allocator, (try wasm_file.stat()).size);
    defer allocator.free(wasm);
    _ = sdk.setLogFile("extism.log", "debug");
    var context = Context.init();
    defer context.deinit();

    var plugin = try Plugin.init(&context, wasm, false);
    defer plugin.deinit();

    var config = std.StringHashMap([]const u8).init(allocator);
    defer config.deinit();
    try config.put("thing", "this is a really important thing");
    try plugin.setConfig(allocator, config);

    const input = "aeiouAEIOU____________________________________&smtms_y?" ** 1182;
    if (plugin.call("count_vowels", input)) |data| {
        std.debug.print("plugin output: {s}", .{data});
    } else |err| switch (err) {
        error.PluginCallError => {
            std.debug.print("plugin returned error: {s}\n", .{plugin.error_info.?});
        },
    }
    std.debug.print("extism version: {s}\n", .{sdk.extismVersion()});
    std.debug.print("has count_vowels: {}\n", .{plugin.hasFunction("count_vowels")});
}
