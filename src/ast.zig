const std = @import("std");
const tokens = @import("tokens.zig");
const Token = tokens.Token;

pub const Statement = union(enum) {
    expression: Expression,
};

pub const Expression = union(enum) {
    literal: Literal,
    unary: *Unary,
    binary: *Binary, // * to break the cycle of a recursive definition

    pub const Literal = struct {
        value: Value,
    };

    pub const Unary = struct {
        operator: Token,
        right: Expression,
    };

    pub const Binary = struct {
        left: Expression,
        operator: Token,
        right: Expression,
    };
};

pub const Value = union(enum) {
    int: f64,
    string: []const u8,
    boolean: bool,
    null,

    pub fn asInt(self: Value) !f64 {
        switch (self) {
            .int => |n| return n,
            else => return error.TypeError,
        }
    }

    pub fn format(self: Value, writer: anytype) !void {
        switch (self) {
            .int => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .null => try writer.writeAll("null"),
        }
    }
};

