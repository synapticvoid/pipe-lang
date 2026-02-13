const std = @import("std");

const ast = @import("ast.zig");
const tok = @import("tokens.zig");
const Value = ast.Value;
const Token = tok.Token;

pub const Environment = struct {
    const Entry = struct {
        value: Value,
        token: Token,
    };

    enclosing: ?*Environment,
    values: std.StringHashMap(Entry),
    allocator: std.mem.Allocator,
    pub fn init(enclosing: ?*Environment, allocator: std.mem.Allocator) Environment {
        return .{
            .enclosing = enclosing,
            .values = std.StringHashMap(Entry).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    pub fn define(self: *Environment, token: Token, value: Value) !void {
        const entry = Entry{ .value = value, .token = token };
        const existing = try self.values.fetchPut(token.lexeme, entry);
        if (existing != null) {
            return error.VariableAlreadyDefined;
        }
    }

    pub fn get(self: *Environment, token: Token) !Value {
        if (self.values.get(token.lexeme)) |entry| {
            return entry.value;
        }

        if (self.enclosing) |enc| {
            return try enc.get(token);
        }

        return error.UndefinedVariable;
    }

    pub fn assign(self: *Environment, token: Token, value: Value) !void {
        if (self.values.contains(token.lexeme)) {
            const entry = Entry{ .value = value, .token = token };
            try self.values.put(token.lexeme, entry);
            return;
        }

        if (self.enclosing) |enc| {
            return try enc.assign(token, value);
        }

        return error.UndefinedVariable;
    }
};
