const std = @import("std");
const json = std.json;

pub fn toCstr(zig_str: []const u8) [*c]const u8 {
    return @ptrCast([*c]const u8, zig_str);
}


pub fn stringifyAlloc(allocator: std.mem.Allocator, value: anytype) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    errdefer list.deinit();
    try stringify(value, .{ .emit_null_optional_fields = false }, list.writer());
    return list.toOwnedSlice();
}

fn stringify(
    value: anytype,
    options: json.StringifyOptions,
    out_stream: anytype,
) @TypeOf(out_stream).Error!void {
    const T = @TypeOf(value);
    if (comptime std.mem.count(u8, @typeName(T), "hash_map.HashMap") > 0 and std.mem.count(u8, @typeName(T), "?") == 0) {
        var it = value.iterator();
        try out_stream.writeByte('{');
        var i: usize = 0;
        while (it.next()) |entry| {
            i += 1;
            const name = entry.key_ptr.*;
            try json.encodeJsonString(name, options, out_stream);
            if (i < value.count()) try out_stream.writeByte(':');
            const val = entry.value_ptr.*;
            try json.encodeJsonString(val, options, out_stream);
        }
        try out_stream.writeByte('}');
        return;
    }

    switch (@typeInfo(T)) {
        .Float, .ComptimeFloat => {
            return std.fmt.formatFloatScientific(value, std.fmt.FormatOptions{}, out_stream);
        },
        .Int, .ComptimeInt => {
            return std.fmt.formatIntValue(value, "", std.fmt.FormatOptions{}, out_stream);
        },
        .Bool => {
            return out_stream.writeAll(if (value) "true" else "false");
        },
        .Null => {
            return out_stream.writeAll("null");
        },
        .Optional => {
            if (value) |payload| {
                return try stringify(payload, options, out_stream);
            } else {
                return try stringify(null, options, out_stream);
            }
        },
        .Enum => {
            if (comptime std.meta.trait.hasFn("jsonStringify")(T)) {
                return value.jsonStringify(options, out_stream);
            }

            @compileError("Unable to stringify enum '" ++ @typeName(T) ++ "'");
        },
        .Union => {
            if (comptime std.meta.trait.hasFn("jsonStringify")(T)) {
                return value.jsonStringify(options, out_stream);
            }

            const info = @typeInfo(T).Union;
            if (info.tag_type) |UnionTagType| {
                inline for (info.fields) |u_field| {
                    if (value == @field(UnionTagType, u_field.name)) {
                        return try stringify(@field(value, u_field.name), options, out_stream);
                    }
                }
            } else {
                @compileError("Unable to stringify untagged union '" ++ @typeName(T) ++ "'");
            }
        },
        .Struct => |S| {
            if (comptime std.meta.trait.hasFn("jsonStringify")(T)) {
                return value.jsonStringify(options, out_stream);
            }

            try out_stream.writeByte('{');
            var field_output = false;
            var child_options = options;
            if (child_options.whitespace) |*child_whitespace| {
                child_whitespace.indent_level += 1;
            }
            inline for (S.fields) |Field| {
                // don't include void fields
                if (Field.type == void) continue;

                var emit_field = true;

                // don't include optional fields that are null when emit_null_optional_fields is set to false
                if (@typeInfo(Field.type) == .Optional) {
                    if (options.emit_null_optional_fields == false) {
                        if (@field(value, Field.name) == null) {
                            emit_field = false;
                        }
                    }
                }

                if (emit_field) {
                    if (!field_output) {
                        field_output = true;
                    } else {
                        try out_stream.writeByte(',');
                    }
                    if (child_options.whitespace) |child_whitespace| {
                        try child_whitespace.outputIndent(out_stream);
                    }
                    try json.encodeJsonString(Field.name, options, out_stream);
                    try out_stream.writeByte(':');
                    if (child_options.whitespace) |child_whitespace| {
                        if (child_whitespace.separator) {
                            try out_stream.writeByte(' ');
                        }
                    }
                    try stringify(@field(value, Field.name), child_options, out_stream);
                }
            }
            if (field_output) {
                if (options.whitespace) |whitespace| {
                    try whitespace.outputIndent(out_stream);
                }
            }
            try out_stream.writeByte('}');
            return;
        },
        .ErrorSet => return stringify(@as([]const u8, @errorName(value)), options, out_stream),
        .Pointer => |ptr_info| switch (ptr_info.size) {
            .One => switch (@typeInfo(ptr_info.child)) {
                .Array => {
                    const Slice = []const std.meta.Elem(ptr_info.child);
                    return stringify(@as(Slice, value), options, out_stream);
                },
                else => {
                    // TODO: avoid loops?
                    std.debug.print("a: {}", .{std.mem.indexOf(u8, @typeName(T), "HashMap") != null});
                    // return stringify(value.*, options, out_stream);
                },
            },
            // TODO: .Many when there is a sentinel (waiting for https://github.com/ziglang/zig/pull/3972)
            .Slice => {
                if (ptr_info.child == u8 and options.string == .String and std.unicode.utf8ValidateSlice(value)) {
                    try json.encodeJsonString(value, options, out_stream);
                    return;
                }

                try out_stream.writeByte('[');
                var child_options = options;
                if (child_options.whitespace) |*whitespace| {
                    whitespace.indent_level += 1;
                }
                for (value) |x, i| {
                    if (i != 0) {
                        try out_stream.writeByte(',');
                    }
                    if (child_options.whitespace) |child_whitespace| {
                        try child_whitespace.outputIndent(out_stream);
                    }
                    try stringify(x, child_options, out_stream);
                }
                if (value.len != 0) {
                    if (options.whitespace) |whitespace| {
                        try whitespace.outputIndent(out_stream);
                    }
                }
                try out_stream.writeByte(']');
                return;
            },
            else => {
                @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'");
            },
        },
        .Array => return stringify(&value, options, out_stream),
        .Vector => |info| {
            const array: [info.len]info.child = value;
            return stringify(&array, options, out_stream);
        },
        else => {
            @compileError("Unable to stringify type '" ++ @typeName(T) ++ "'");
        },
    }
    unreachable;
}
