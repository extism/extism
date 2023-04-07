const std = @import("std");
const c = @import("ffi.zig");

const Self = @This();
c_func: ?*c.ExtismFunction,

pub fn init(name: []const u8, inputs: []const c.ExtismValType, outputs: []const c.ExtismValType, f: c.ExtismFunctionType, user_data: ?*anyopaque) Self {
    var inputsPtr: ?*const c.ExtismValType = null;
    if (inputs.len > 0) {
        inputsPtr = &inputs[0];
    }
    var outputsPtr: ?*const c.ExtismValType = null;
    if (outputs.len > 0) {
        outputsPtr = &outputs[0];
    }
    const ptr = c.extism_function_new(name.ptr, inputsPtr, @as(u64, inputs.len), outputsPtr, @as(u64, outputs.len), f, user_data, null);

    return .{ .c_func = ptr };
}

pub fn deinit(self: *Self) void {
    c.extism_function_free(self.c_func);
    self.c_func = null;
}

pub fn setNamespace(self: *Self, namespace: []const u8) void {
    c.extism_function_set_namespace(self.c_func, namespace.ptr);
}

pub fn withNamespace(self: Self, namespace: []const u8) Self {
    var not_so_self = self;
    not_so_self.setNamespace(namespace);
    return not_so_self;
}
