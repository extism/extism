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

const wasmfile_manifest = manifest.WasmFile{ .path = "../wasm/code-functions.wasm" };
const man = .{ .wasm = &[_]manifest.Wasm{.{ .wasm_file = wasmfile_manifest }} };

test "Single threaded tests" {
    var wasm_start = try std.time.Timer.start();
    _ = sdk.setLogFile("test.log", .Debug);

    var ctx = Context.init();
    defer ctx.deinit();

    var f = Function.init(
        "hello_world",
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &hello_world,
        @constCast(@ptrCast(*const anyopaque, "user data")),
    );
    defer f.deinit();

    var plugin = try Plugin.initFromManifest(testing.allocator, &ctx, man, &[_]Function{f}, true);
    defer plugin.deinit();

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
    const S = struct {
        fn _test() !void {
            var ctx = Context.init();
            defer ctx.deinit();
            var f = Function.init(
                "hello_world",
                &[_]sdk.c.ExtismValType{sdk.c.I64},
                &[_]sdk.c.ExtismValType{sdk.c.I64},
                &hello_world,
                @constCast(@ptrCast(*const anyopaque, "user data")),
            );
            defer f.deinit();
            var plugin = try Plugin.initFromManifest(testing.allocator, &ctx, man, &[_]Function{f}, true);
            defer plugin.deinit();
            const output = try plugin.call("count_vowels", "this is a test");
            std.debug.print("{s}\n", .{output});
        }
    };
    const t1 = try std.Thread.spawn(.{}, S._test, .{});
    const t2 = try std.Thread.spawn(.{}, S._test, .{});
    t1.join();
    t2.join();
    _ = sdk.setLogFile("test.log", .Debug);
    var ctx = Context.init();
    defer ctx.deinit();

    var f = Function.init(
        "hello_world",
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &hello_world,
        @constCast(@ptrCast(*const anyopaque, "user data")),
    );
    defer f.deinit();

    var plugin = try Plugin.initFromManifest(testing.allocator, &ctx, man, &[_]Function{f}, true);
    defer plugin.deinit();

    const output = try plugin.call("count_vowels", "this is a test");
    std.debug.print("{s}\n", .{output});
}

test "Plugin Cancellation" {
    const loop_manifest = manifest.WasmFile{ .path = "../wasm/loop.wasm" };
    const loop_man = .{ .wasm = &[_]manifest.Wasm{.{ .wasm_file = loop_manifest }} };
    var ctx = Context.init();
    defer ctx.deinit();
    _ = sdk.setLogFile("test.log", .Debug);
    var f = Function.init(
        "hello_world",
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &[_]sdk.c.ExtismValType{sdk.c.I64},
        &hello_world,
        @constCast(@ptrCast(*const anyopaque, "user data")),
    );
    defer f.deinit();

    var plugin = try Plugin.initFromManifest(testing.allocator, &ctx, loop_man, &[_]Function{f}, true);
    defer plugin.deinit();
    var handle = plugin.cancelHandle();
    const S = struct {
        fn _test(h: *sdk.CancelHandle) void {
            std.os.nanosleep(1, 0);
            _ = h.cancel();
        }
    };
    _ = try std.Thread.spawn(.{}, S._test, .{&handle});
    var call_start = try std.time.Timer.start();
    const output = plugin.call("infinite_loop", "abc123");
    const call_end = call_start.read();
    try std.testing.expectError(error.PluginCallFailed, output);
    std.debug.print("Cancelled plugin ran for {}\n", .{std.fmt.fmtDuration(call_end)});
}
