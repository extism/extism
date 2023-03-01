const c = @import("ffi.zig");
const Self = @This();

handle: *const c.ExtismCancelHandle,

pub fn cancel(self: *Self) bool {
    return c.extism_plugin_cancel(self.handle);
}
