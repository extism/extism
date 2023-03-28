const std = @import("std");
const testing = std.testing;
pub const c = @import("ffi.zig");

pub const Context = @import("context.zig");
pub const Plugin = @import("plugin.zig");
pub const CurrentPlugin = @import("current_plugin.zig");
pub const CancelHandle = @import("cancel_handle.zig");
pub const Function = @import("function.zig");
pub const manifest = @import("manifest.zig");
pub const LogLevel = enum {
    Error,
    Warn,
    Info,
    Debug,
    Trace,
};

pub fn setLogFile(file_name: []const u8, level: LogLevel) bool {
    const res = c.extism_log_file(file_name.ptr, @tagName(level));
    return res;
}

pub fn extismVersion() []const u8 {
    const c_version = c.extism_version();
    const version = std.mem.span(c_version);
    return version;
}
