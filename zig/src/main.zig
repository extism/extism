const std = @import("std");
const testing = std.testing;
const c = @import("ffi.zig");
const utils = @import("utils.zig");
const toCstr = utils.toCstr;

pub const Context = @import("context.zig").Context;
pub const Plugin = @import("plugin.zig").Plugin;

pub const WasmData = struct {
    data: []const u8,
    hash: []const u8,
    name: []const u8,
};

pub const WasmFile = struct { path: []const u8, hash: []const u8, name: []const u8 };

pub const WasmUrl = struct {
    url: []const u8,
    hash: []const u8,
    name: []const u8,
    method: []const u8,
    headers: std.StringHashMap([]const u8),
};

pub fn Manifest(comptime T: type) type {
    return struct {
        wasm: T,
        memory: struct { max_pages: u32 },
        config: std.StringHashMap([]const u8),
        allowed_hosts: [][]const u8,
        allowed_paths: std.StringHashMap([]const u8),
        timeout: usize,
    };
}

pub fn setLogFile(file_name: []const u8, level: []const u8) bool {
    const res = c.extism_log_file(toCstr(file_name), toCstr(level));
    return res;
}

pub fn extismVersion() []const u8 {
    const c_version = c.extism_version();
    const version = std.mem.span(c_version);
    return version;
}
