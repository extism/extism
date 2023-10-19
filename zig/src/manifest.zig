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
    headers: ?std.json.ArrayHashMap([]const u8) = null,
};

pub const Wasm = union(enum) {
    wasm_data: WasmData,
    wasm_file: WasmFile,
    wasm_url: WasmUrl,
    pub fn jsonStringify(self: @This(), jws: anytype) !void {
        switch (self) {
            inline else => |value| {
                try jws.write(value);
            },
        }
    }
};

pub const Manifest = struct {
    wasm: []const Wasm,
    memory: ?struct { max_pages: ?u32 } = null,
    config: ?std.json.ArrayHashMap([]const u8) = null,
    allowed_hosts: ?[]const []const u8 = null,
    allowed_paths: ?std.json.ArrayHashMap([]const u8) = null,
    timeout: ?usize = null,
};
