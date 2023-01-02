const std = @import("std");
const Context = @import("context.zig").Context;
const Manifest = @import("manifest.zig").Manifest;
const c = @import("ffi.zig");
const utils = @import("utils.zig");
const toCstr = utils.toCstr;

pub const Plugin = struct {
    ctx: *Context,
    id: i32,

    // We have to use this until ziglang/zig#2647 is resolved.
    error_info: ?[]const u8,

    /// Create a new plugin from a WASM module
    pub fn init(ctx: *Context, data: []const u8, wasi: bool) !Plugin {
        ctx.mutex.lock();
        defer ctx.mutex.unlock();
        const plugin = c.extism_plugin_new(ctx.ctx, toCstr(data), @as(u64, data.len), wasi);
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
    pub fn initFromManifest(allocator: std.mem.Allocator, ctx: *Context, manifest: Manifest, wasi: bool) !Plugin {
        const json = try utils.stringifyAlloc(allocator, manifest);
        defer allocator.free(json);
        return init(ctx, json, wasi);
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
        const res = c.extism_plugin_call(self.ctx.ctx, self.id, toCstr(function_name), toCstr(input), @intCast(u64, input.len));
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
        const res = c.extism_plugin_update(self.ctx.ctx, self.id, toCstr(data), @intCast(u64, data.len), wasi);
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
        _ = c.extism_plugin_config(self.ctx.ctx, self.id, toCstr(config_json), @intCast(u64, config_json.len));
    }

    /// Returns true if the plugin has a function matching `name`
    pub fn hasFunction(self: Plugin, function_name: []const u8) bool {
        self.ctx.mutex.lock();
        defer self.ctx.mutex.unlock();
        const res = c.extism_plugin_function_exists(self.ctx.ctx, self.id, toCstr(function_name));
        return res;
    }
};
