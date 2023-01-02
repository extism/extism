const std = @import("std");
const testing = std.testing;
const c = @import("ffi.zig");
const utils = @import("utils.zig");
const toCstr = utils.toCstr;

pub const Context = @import("context.zig").Context;
pub const Plugin = @import("plugin.zig").Plugin;
pub const manifest = @import("manifest.zig");
pub const LogLevel = enum {
    Error,
    Warn,
    Info,
    Debug,
    Trace,

    fn toStr(self: LogLevel) []const u8 {
        inline for (@typeInfo(LogLevel).Enum.fields) |field| {
            if (@enumToInt(self) == field.value) {
                const first_lower = std.ascii.toLower(field.name[0]);
                return .{first_lower} ++  field.name[1..];
            }
        }
        unreachable;
    }
};

pub fn setLogFile(file_name: []const u8, level: LogLevel) bool {
    const res = c.extism_log_file(toCstr(file_name), toCstr(level.toStr()));
    return res;
}

pub fn extismVersion() []const u8 {
    const c_version = c.extism_version();
    const version = std.mem.span(c_version);
    return version;
}
