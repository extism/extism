const std = @import("std");
const Context = @import("context.zig");
const Manifest = @import("manifest.zig").Manifest;
const Function = @import("function.zig");
const CancelHandle = @import("cancel_handle.zig");
const c = @import("ffi.zig");

const Self = @This();

ctx: *Context,
owns_context: bool,
id: i32,

// We have to use this until ziglang/zig#2647 is resolved.
error_info: ?[]const u8,

/// Create a new plugin from a WASM module
pub fn init(allocator: std.mem.Allocator, ctx: *Context, data: []const u8, functions: []const Function, wasi: bool) !Self {
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
        .owns_context = false,
    };
}

/// Create a new plugin from the given manifest
pub fn initFromManifest(allocator: std.mem.Allocator, ctx: *Context, manifest: Manifest, functions: []const Function, wasi: bool) !Self {
    const json = try std.json.stringifyAlloc(allocator, manifest, .{ .emit_null_optional_fields = false });
    defer allocator.free(json);
    return init(allocator, ctx, json, functions, wasi);
}

/// Create a new plugin from a WASM module in its own context
pub fn create(allocator: std.mem.Allocator, data: []const u8, functions: []const Function, wasi: bool) !Self {
    const ctx = Context.init();
    var plugin = init(allocator, ctx, data, functions, wasi);
    plugin.owns_context = true;
    return plugin;
}

/// Create a new plugin from the given manifest in its own context
pub fn createFromManifest(allocator: std.mem.Allocator, manifest: Manifest, functions: []const Function, wasi: bool) !Self {
    const json = try std.json.stringifyAlloc(allocator, manifest, .{ .emit_null_optional_fields = false });
    defer allocator.free(json);
    return create(allocator, json, functions, wasi);
}

pub fn deinit(self: *Self) void {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    c.extism_plugin_free(self.ctx.ctx, self.id);
    if (self.owns_context) {
        self.ctx.deinit();
    }
}

pub fn cancelHandle(self: *Self) CancelHandle {
    const ptr = c.extism_plugin_cancel_handle(self.ctx.ctx, self.id);
    return .{ .handle = ptr };
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
    const json = try std.json.stringifyAlloc(allocator, manifest, .{ .emit_null_optional_fields = false });
    defer allocator.free(json);
    return self.update(json, wasi);
}
/// Set configuration values
pub fn setConfig(self: *Self, allocator: std.mem.Allocator, config: std.json.ArrayHashMap([]const u8)) !void {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    const config_json = try std.json.stringifyAlloc(allocator, config, .{ .emit_null_optional_fields = false });
    defer allocator.free(config_json);
    _ = c.extism_plugin_config(self.ctx.ctx, self.id, config_json.ptr, @as(u64, config_json.len));
}

/// Returns true if the plugin has a function matching `function_name`
pub fn hasFunction(self: Self, function_name: []const u8) bool {
    self.ctx.mutex.lock();
    defer self.ctx.mutex.unlock();
    const res = c.extism_plugin_function_exists(self.ctx.ctx, self.id, function_name.ptr);
    return res;
}
