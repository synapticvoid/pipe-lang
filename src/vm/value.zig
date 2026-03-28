const std = @import("std");
const activeTag = std.meta.activeTag;
const RuntimeContext = @import("../runtime.zig").RuntimeContext;

pub const NativeFn = struct {
    pub const Func = *const fn (args: []const Value, ctx: RuntimeContext) Value;

    name: []const u8,
    arity: ?u8, // null = variadic
    func: Func,
};

pub const Value = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    null,
    unit,

    // Index of the VM function
    // Stored in the function table in Program
    function: u16,

    native: NativeFn,

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
            .function => |a| a == other.function,
            .native => |a| a.func == other.native.func,
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
            .function, .native => true,
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
            .function => try writer.writeAll("fn"),
            .native => |b| try writer.print("<builtin {s}>", .{b.name}),
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
        }
    }
};
