const std = @import("std");
const testing = std.testing;
const c = @import("ffi.zig");
const utils = @import("utils.zig");
const toCstr = utils.toCstr;

pub const Context = @import("context.zig").Context;
pub const Plugin = @import("plugin.zig").Plugin;
pub const manifest = @import("manifest.zig");


pub fn setLogFile(file_name: []const u8, level: []const u8) bool {
    const res = c.extism_log_file(toCstr(file_name), toCstr(level));
    return res;
}

pub fn extismVersion() []const u8 {
    const c_version = c.extism_version();
    const version = std.mem.span(c_version);
    return version;
}
