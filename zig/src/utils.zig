pub fn toCstr(zig_str: []const u8) [*c]const u8 {
    return @ptrCast([*c]const u8, zig_str);
}