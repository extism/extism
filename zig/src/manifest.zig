const std = @import("std");

pub const WasmData = struct {
    data: []const u8,
    hash: ?[]const u8 = null,
    name: ?[]const u8 = null,
};

pub const WasmFile = struct { path: []const u8, hash: ?[]const u8 = null, name: ?[]const u8 = null };

pub const WasmUrl = struct {
    url: []const u8,
    hash: ?[]const u8 = null,
    name: ?[]const u8 = null,
    method: ?[]const u8 = null,
    headers: ?std.StringHashMap([]const u8) = null,
};

/// Creates a new Manifest
/// T must be one of `WasmData`, `WasmFile`, `WasmUrl`
pub fn Manifest(comptime T: type) type {
    return struct {
        wasm: []const T,
        memory: ?struct { max_pages: ?u32 } = null,
        config: ?std.StringHashMap([]const u8) = null,
        allowed_hosts: ?[][]const u8 = null,
        allowed_paths: ?std.StringHashMap([]const u8) = null,
        timeout: ?usize = null,
    };
}
