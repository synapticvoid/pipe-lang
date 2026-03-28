const std = @import("std");
const activeTag = std.meta.activeTag;

const Callable = @import("callable.zig").Callable;
const StructKind = @import("../ast.zig").StructKind;

pub const Value = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    function: Callable,
    null,
    unit,

    // pointer so that copies of a value share identity (u2 = u1 means u1.field == u2.field)
    // var u1 = User("Bob");
    // var u2 = u1;
    // if (u1.name == u2.name) { ... } // true
    struct_instance: *StructInstance,

    pub const StructInstance = struct {
        type_name: []const u8,
        field_names: []const []const u8,
        body_field_values: []Value,
        body_field_names: []const []const u8,
        field_values: []Value,
        kind: StructKind,
    };

    pub fn eql(self: Value, other: Value) bool {
        const self_tag = activeTag(self);
        const other_tag = activeTag(other);
        if (self_tag != other_tag) {
            return false;
        }

        return switch (self) {
            .int => |a| a == other.int,
            .string => |a| std.mem.eql(u8, a, other.string),
            .boolean => |a| a == other.boolean,
            .null, .unit => true,
            .struct_instance => |a| {
                const b = other.struct_instance;
                if (a.kind == .plain) {
                    return a == b;
                }
                if (!std.mem.eql(u8, a.type_name, b.type_name)) {
                    return false;
                }
                for (a.field_values, b.field_values) |av, bv| {
                    if (!av.eql(bv)) {
                        return false;
                    }
                }
                return true;
            },
            else => false,
        };
    }

    pub fn asInt(self: Value) !i64 {
        switch (self) {
            .int => |n| return n,
            else => return error.TypeError,
        }
    }

    // Returns true if the value is truthy
    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .null, .unit => false,
            .boolean => |b| b,
            .int => |n| n != 0,
            .function => true,
            .string => |s| s.len > 0,
            .struct_instance => |_| true,
        };
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .int => true,
            else => false,
        };
    }

    pub fn format(self: Value, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .int => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .function => |f| switch (f) {
                .user => |u| try writer.print("fn<{s}>", .{u.declaration.name.lexeme}),
                .builtin => |bi| try writer.print("fn<{s}>", .{bi.name}),
                .struct_constructor => |sc| try writer.print("fn<{s}>", .{sc.name}),
            },
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
            .struct_instance => |si_ptr| {
                const si = si_ptr.*;
                switch (si.kind) {
                    .plain => try writer.print("<{s}>", .{si.type_name}),
                    .case => {
                        try writer.print("{s}(", .{si.type_name});
                        for (si.field_names, si.field_values, 0..) |name, value, i| {
                            if (i > 0) {
                                try writer.writeAll(", ");
                            }
                            try writer.print("{s}=", .{name});
                            try value.format(writer);
                        }
                        try writer.writeAll(")");
                    },
                }
            },
        }
    }
};
