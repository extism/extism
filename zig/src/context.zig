const std = @import("std");
const c = @import("ffi.zig");

const Self = @This();

mutex: std.Thread.Mutex,
ctx: *c.ExtismContext,

// We have to use this until ziglang/zig#2647 is resolved.
error_info: ?[]const u8,

/// Creates a new context, it should be freed using `deinit`
pub fn init() Self {
    const new_ctx = c.extism_context_new();
    return .{
        .mutex = .{},
        .ctx = new_ctx orelse unreachable,
        .error_info = null,
    };
}

// Free a context
pub fn deinit(self: Self) void {
    c.extism_context_free(self.ctx);
}

pub fn reset(self: *Self) void {
    self.mutex.lock();
    defer self.mutex.unlock();
    c.extism_context_reset(self.ctx);
}
