const std = @import("std");
const Context = @import("context.zig");
const Manifest = @import("manifest.zig").Manifest;
const Function = @import("function.zig");
const CancelHandle = @import("cancel_handle.zig");
const c = @import("ffi.zig");
const utils = @import("utils.zig");

const Self = @This();

ctx: *Context,
id: i32,

// We have to use this until ziglang/zig#2647 is resolved.
error_info: ?[]const u8,

/// Create a new plugin from a WASM module
pub fn init(allocator: std.mem.Allocator, ctx: *Context, data: []const u8, functions: []Function, wasi: bool) !Self {
    ctx.mutex.lock();
    defer ctx.mutex.unlock();
    var plugin: i32 = -1;
    if (functions.len > 0) {
        var funcPtrs = try allocator.alloc(?*c.ExtismFunction, functions.len);
        defer allocator.free(funcPtrs);
        var i: usize = 0;
        for (functions) |function| {
            funcPtrs[i] = function.c_func;
            i += 1;
        }
        plugin = c.extism_plugin_new(ctx.ctx, data.ptr, @as(u64, data.len), &funcPtrs[0], functions.len, wasi);
    } else {
        plugin = c.extism_plugin_new(ctx.ctx, data.ptr, @as(u64, data.len), null, 0, wasi);
    }

    if (plugin < 0) {
        const err_c = c.extism_error(ctx.ctx, @as(i32, -1));
        const err = std.mem.span(err_c);
        if (!std.mem.eql(u8, err, "")) {
            ctx.error_info = err;
        }
        ctx.error_info = "Unknown";
        return error.PluginLoadFailed;
    }
    return Self{
        .id = plugin,
        .ctx = ctx,
        .error_info = null,
    };
}

/// Create a new plugin from the given manifest
pub fn initFromManifest(allocator: std.mem.Allocator, ctx: *Context, manifest: Manifest, functions: []Function, wasi: bool) !Self {
    const json = try utils.stringifyAlloc(allocator, manifest);
    defer allocator.free(json);
    return init(allocator, ctx, json, functions, wasi);
}

pub fn deinit(self: *Self) void {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    c.extism_plugin_free(self.ctx.ctx, self.id);
}

pub fn cancelHandle(self: *Self) CancelHandle {
    const ptr = c.extism_plugin_cancel_handle(self.ctx.ctx, self.id);
    return CancelHandle{ .handle = ptr };
}

/// Call a function with the given input
pub fn call(self: *Self, function_name: []const u8, input: []const u8) ![]const u8 {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    const res = c.extism_plugin_call(self.ctx.ctx, self.id, function_name.ptr, input.ptr, @as(u64, input.len));
    if (res != 0) {
        var err_c = c.extism_error(self.ctx.ctx, self.id);
        const err = std.mem.span(err_c);

        if (!std.mem.eql(u8, err, "")) {
            self.error_info = err;
        }
        self.error_info = "<unset by plugin>";
        return error.PluginCallFailed;
    }

    const len = c.extism_plugin_output_length(self.ctx.ctx, self.id);

    if (len > 0) {
        const output_data = c.extism_plugin_output_data(self.ctx.ctx, self.id);
        return output_data[0..len];
    }
    return "";
}

/// Update a plugin with the given WASM module
pub fn update(self: *Self, data: []const u8, wasi: bool) !void {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    const res = c.extism_plugin_update(self.ctx.ctx, self.id, data.ptr, @as(u64, data.len), null, 0, wasi);
    if (res) return;
    const err_c = c.extism_error(self.ctx.ctx, @as(i32, -1));
    const err = std.mem.span(err_c);
    if (!std.mem.eql(u8, err, "")) {
        self.error_info = err;
    }
    self.error_info = "Unknown";
    return error.PluginUpdateFailed;
}

/// Update a plugin with the given manifest
pub fn updateWithManifest(self: *Self, allocator: std.mem.Allocator, manifest: Manifest, wasi: bool) !void {
    const json = try utils.stringifyAlloc(allocator, manifest);
    defer allocator.free(json);
    return self.update(json, wasi);
}
/// Set configuration values
pub fn setConfig(self: *Self, allocator: std.mem.Allocator, config: std.StringHashMap([]const u8)) !void {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    const config_json = try utils.stringifyAlloc(allocator, config);
    defer allocator.free(config_json);
    _ = c.extism_plugin_config(self.ctx.ctx, self.id, config_json.ptr, @as(u64, config_json.len));
}

/// Returns true if the plugin has a function matching `name`
pub fn hasFunction(self: Self, function_name: []const u8) bool {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    const res = c.extism_plugin_function_exists(self.ctx.ctx, self.id, function_name.ptr);
    return res;
}
