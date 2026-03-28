const std = @import("std");
const activeTag = std.meta.activeTag;

pub const Value = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    null,
    unit,

    // Index of the VM function
    // Stored in the function table in Program
    vm_function: u16,

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
            .vm_function => |a| a == other.vm_function,
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
            .string => |s| s.len > 0,
            .vm_function => true,
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
            .vm_function => try writer.writeAll("fn"),
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
        }
    }
};
