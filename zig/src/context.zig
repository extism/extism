const std = @import("std");
const c = @import("ffi.zig");
const utils = @import("utils.zig");
const toCstr = utils.toCstr;

pub const Context = struct {
    mutex: std.Thread.Mutex,
    ctx: *c.ExtismContext,

    // We have to use this until ziglang/zig#2647 is resolved.
    error_info: ?[]const u8,

    /// Creates a new context, it should be freed using `deinit`
    pub fn init() Context {
        const new_ctx = c.extism_context_new();
        return Context{
            .mutex = .{},
            .ctx = new_ctx orelse unreachable,
            .error_info = null,
        };
    }

    // Free a context
    pub fn deinit(self: Context) void {
        c.extism_context_free(self.ctx);
    }

    pub fn reset(self: *Context) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        c.extism_context_reset(self.ctx);
    }
};
