pub usingnamespace @cImport({
    @cInclude("extism.h");
});

pub const EditedFunctions = struct {
    /// DIFFERENCE: exports: [*c][*c]u8 -> [*c]const [*c]const u8
    /// Edited for avoid doing this mess: @ptrCast([*c][*c]u8, @qualCast([*c][*c]const u8, @ptrCast([*c]const [*c]const u8, exports)));
    pub extern fn extism_plugin_export_list_free(exports: [*c]const [*c]const u8, len: u64) void;
};