const std = @import("std");
const testing = std.testing;
const sdk = @import("extism");
const Context = sdk.Context;
const Plugin = sdk.Plugin;
const manifest = sdk.manifest;

test "Single threaded tests" {
    const dir = try std.fs.cwd().openDir("../wasm", .{});
    const wasm_file = try dir.openFile("code.wasm", .{});
    const wasm = try wasm_file.readToEndAlloc(testing.allocator, (try wasm_file.stat()).size);
    defer testing.allocator.free(wasm);

    var wasm_start = try std.time.Timer.start();
    _ = sdk.setLogFile("test.log", .Debug);

    var ctx = Context.init();
    var plugin = try Plugin.init(&ctx, wasm, false);
    std.debug.print("\nregister loaded plugin: {}\n", .{std.fmt.fmtDuration(wasm_start.read())});
    const repeat = 1182;
    const input = "aeiouAEIOU____________________________________&smtms_y?" ** repeat;
    const data = try plugin.call("count_vowels", input);
    try testing.expectEqualStrings("{\"count\": 11820}", data);
    std.debug.print("register plugin + function call: {}, sent input size: {} bytes\n", .{ std.fmt.fmtDuration(wasm_start.read()), input.len });
    std.debug.print("--------------\n", .{});
    var i: usize = 0;
    var wasm_elapsed: u64 = 0;
    while (i < 100) : (i += 1) {
        var call_start = try std.time.Timer.start();
        _ = try plugin.call("count_vowels", input);
        wasm_elapsed += call_start.read();
    }
    const wasm_avg = wasm_elapsed / i;

    i = 0;
    var native_elapsed: u64 = 0;
    var native_count: u32 = 0;
    while (i < 100) : (i += 1) {
        var native_start = try std.time.Timer.start();
        for (input) |char| {
            switch (char) {
                'A', 'I', 'E', 'O', 'U', 'a', 'e', 'i', 'o', 'u' => native_count += 1,
                else => {},
            }
        }
        native_elapsed += native_start.read();
    }
    const native_avg = native_elapsed / i;
    std.debug.print("native function call (avg, N = {}): {}\n", .{ i, std.fmt.fmtDuration(native_avg) });
    std.debug.print("wasm function call (avg, N = {}): {}\n", .{ i, std.fmt.fmtDuration(wasm_avg) });
}

test "Multi threaded tests" {
    const stdout = std.io.getStdOut().writer();
    const dir = try std.fs.cwd().openDir("../wasm", .{});
    const wasm_file = try dir.openFile("code.wasm", .{});
    const wasm = try wasm_file.readToEndAlloc(testing.allocator, (try wasm_file.stat()).size);
    defer testing.allocator.free(wasm);
    const S = struct {
        fn _test(w: []const u8) !void {
            var ctx = Context.init();
            var plugin = try Plugin.init(&ctx, w, false);
            const output = try plugin.call("count_vowels", "this is a test");
            const local_stdout = std.io.getStdOut().writer();
            try local_stdout.writeAll(output);
            try local_stdout.writeByte('\n');
        }
    };
    try stdout.writeByte('\n');
    _ = try std.Thread.spawn(.{}, S._test, .{wasm});
    _ = try std.Thread.spawn(.{}, S._test, .{wasm});
    var ctx = Context.init();
    var plugin = try Plugin.init(&ctx, wasm, false);
    const output = try plugin.call("count_vowels", "this is a test");
    try stdout.writeAll(output);
    try stdout.writeByte('\n');
}
