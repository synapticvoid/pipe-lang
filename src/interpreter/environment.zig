const std = @import("std");

const Value = @import("value.zig").Value;

pub const Environment = struct {
    enclosing: ?*Environment,
    values: std.StringHashMapUnmanaged(Value),
    allocator: std.mem.Allocator,

    pub fn init(enclosing: ?*Environment, allocator: std.mem.Allocator) Environment {
        return .{
            .enclosing = enclosing,
            .values = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit(self.allocator);
    }

    // Creates a new binding in the current scope; errors if the name is already defined here.
    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        const gop = try self.values.getOrPut(self.allocator, name);
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
            try self.values.put(self.allocator, name, value);
            return;
        }

        if (self.enclosing) |enc| {
            return try enc.assign(name, value);
        }

        return error.UndefinedVariable;
    }
};
