const std = @import("std");
const c = @import("ffi.zig");

c_currplugin: *c.ExtismCurrentPlugin,

const Self = @This();

pub fn getCurrentPlugin(ptr: *c.ExtismCurrentPlugin) Self {
    return .{ .c_currplugin = ptr };
}

pub fn getMemory(self: Self, offset: u64) []const u8 {
    const len = c.extism_current_plugin_memory_length(self.c_currplugin, offset);
    const c_data = c.extism_current_plugin_memory(self.c_currplugin);
    const data: [*:0]u8 = std.mem.span(c_data);
    return data[offset .. offset + len];
}

pub fn alloc(self: *Self, n: u64) u64 {
    return c.extism_current_plugin_memory_alloc(self.c_currplugin, n);
}

pub fn free(self: *Self, offset: u64) void {
    c.extism_current_plugin_memory_free(self.c_currplugin, offset);
}

pub fn length(self: *Self, offset: u64) u64 {
    return c.extism_current_plugin_memory_length(self.c_currplugin, offset);
}

pub fn returnBytes(self: *Self, val: *const c.ExtismVal, data: []const u8) void {
    const mem = self.alloc(@as(u64, data.len));
    const ptr = self.getMemory(mem);
    @memcpy(ptr, data);
    val.v.i64 = @as(i64, mem);
}

pub fn inputBytes(self: *Self, val: *const c.ExtismVal) []const u8 {
    return self.getMemory(@intCast(val.v.i64));
}
