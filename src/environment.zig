const std = @import("std");

const ast = @import("ast.zig");
const tok = @import("tokens.zig");
const Value = ast.Value;
const Token = tok.Token;

pub const Environment = struct {
    enclosing: ?*Environment,
    values: std.StringHashMap(Value),
    allocator: std.mem.Allocator,

    pub fn init(enclosing: ?*Environment, allocator: std.mem.Allocator) Environment {
        return .{
            .enclosing = enclosing,
            .values = std.StringHashMap(Value).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        const existing = try self.values.fetchPut(name, value);
        if (existing != null) {
            return error.VariableAlreadyDefined;
        }
    }

    pub fn get(self: *Environment, name: []const u8) !Value {
        if (self.values.get(name)) |value| {
            return value;
        }

        if (self.enclosing) |enc| {
            return try enc.get(name);
        }

        return error.UndefinedVariable;
    }

    pub fn assign(self: *Environment, name: []const u8, value: Value) !void {
        if (self.values.contains(name)) {
            try self.values.put(name, value);
            return;
        }

        if (self.enclosing) |enc| {
            return try enc.assign(name, value);
        }

        return error.UndefinedVariable;
    }
};
