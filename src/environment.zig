const std = @import("std");

const ast = @import("ast.zig");
const Value = ast.Value;

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

    // Creates a new binding in the current scope; errors if the name is already defined here.
    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        const gop = try self.values.getOrPut(name);
        if (gop.found_existing) {
            return error.VariableAlreadyDefined;
        }
        gop.value_ptr.* = value;
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

    // Mutates an existing binding anywhere in the scope chain; errors if not found anywhere.
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
