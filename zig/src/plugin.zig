const std = @import("std");
const Context = @import("context.zig").Context;
const Manifest = @import("manifest.zig").Manifest;
const Function = @import("function.zig");
const c = @import("ffi.zig");
const utils = @import("utils.zig");

pub const Plugin = struct {
    ctx: *Context,
    id: i32,

    // We have to use this until ziglang/zig#2647 is resolved.
    error_info: ?[]const u8,

    /// Create a new plugin from a WASM module
    pub fn init(allocator: std.mem.Allocator, ctx: *Context, data: []const u8, functions: []Function, wasi: bool) !Plugin {
        ctx.mutex.lock();
        defer ctx.mutex.unlock();
        var plugin: i32 = -1;
        if (functions.len > 0) {
            var funcPtrs = try allocator.alloc(?*c.ExtismFunction, functions.len);
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
        return Plugin{
            .id = plugin,
            .ctx = ctx,
            .error_info = null,
        };
    }

    /// Create a new plugin from the given manifest
    pub fn initFromManifest(allocator: std.mem.Allocator, ctx: *Context, manifest: Manifest, functions: []Function, wasi: bool) !Plugin {
        const json = try utils.stringifyAlloc(allocator, manifest);
        defer allocator.free(json);
        return init(allocator, ctx, json, functions, wasi);
    }

    pub fn deinit(self: *Plugin) void {
        self.ctx.mutex.lock();
        defer self.ctx.mutex.unlock();
        c.extism_plugin_free(self.ctx.ctx, self.id);
    }

    /// Call a function with the given input
    pub fn call(self: *Plugin, function_name: []const u8, input: []const u8) ![]const u8 {
        self.ctx.mutex.lock();
        defer self.ctx.mutex.unlock();
        const res = c.extism_plugin_call(self.ctx.ctx, self.id, function_name.ptr, input.ptr, @intCast(u64, input.len));
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

    // TODO: test this function
    /// Update a plugin with the given WASM module
    pub fn update(self: *Plugin, data: []const u8, wasi: bool) !void {
        self.ctx.mutex.lock();
        defer self.ctx.mutex.unlock();
        const res = c.extism_plugin_update(self.ctx.ctx, self.id, data.ptr, @intCast(u64, data.len), null, 0, wasi);
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
    pub fn updateWithManifest(self: *Plugin, allocator: std.mem.Allocator, manifest: Manifest, wasi: bool) !void {
        const json = try utils.stringifyAlloc(allocator, manifest);
        defer allocator.free(json);
        return self.update(json, wasi);
    }
    /// Set configuration values
    pub fn setConfig(self: *Plugin, allocator: std.mem.Allocator, config: std.StringHashMap([]const u8)) !void {
        self.ctx.mutex.lock();
        defer self.ctx.mutex.unlock();
        const config_json = try utils.stringifyAlloc(allocator, config);
        defer allocator.free(config_json);
        _ = c.extism_plugin_config(self.ctx.ctx, self.id,config_json.ptr, @intCast(u64, config_json.len));
    }

    /// Returns true if the plugin has a function matching `name`
    pub fn hasFunction(self: Plugin, function_name: []const u8) bool {
        self.ctx.mutex.lock();
        defer self.ctx.mutex.unlock();
        const res = c.extism_plugin_function_exists(self.ctx.ctx, self.id, function_name.ptr);
        return res;
    }
};

pub const CurrentPlugin = struct {
    c_currplugin: *c.ExtismCurrentPlugin,

    pub fn getCurrentPlugin(ptr: *c.ExtismCurrentPlugin) CurrentPlugin {
        return .{ .c_currplugin = ptr };
    }

    pub fn getMemory(self: CurrentPlugin, offset: u64) []const u8 {
        const len = c.extism_current_plugin_memory_length(self.c_currplugin, offset);
        const c_data = c.extism_current_plugin_memory(self.c_currplugin);
        const data: [*:0]u8 = std.mem.span(c_data);
        return (data + offset)[0..len];
    }

    pub fn alloc(self: *CurrentPlugin, n: u64) u64 {
        return c.extism_current_plugin_memory_alloc(self.c_currplugin, n);
    }

    pub fn free(self: *CurrentPlugin, offset: u64) void {
        c.extism_current_plugin_memory_free(self.c_currplugin, offset);
    }

    pub fn length(self: *CurrentPlugin, offset: u64) u64 {
        return c.extism_current_plugin_memory_length(self.c_currplugin,offset);
    }

    pub fn returnBytes(self: *CurrentPlugin, val: *const c.ExtismVal, data: []const u8) void {
        const mem = self.alloc(@as(u64, data.len));
        const ptr = self.getMemory(mem);
        std.mem.copy(u8, ptr, data);
        val.v.i64 = @as(i64, mem);
    }

    pub fn inputBytes(self: *CurrentPlugin, val: *const c.ExtismVal) []const u8 {
        return self.getMemory(@intCast(u64, val.v.@"i64"));
    }
};
