const std = @import("std");
const ast = @import("ast.zig");
const tokens = @import("tokens.zig");

const Environment = @import("environment.zig").Environment;
const Expression = ast.Expression;
const Value = ast.Value;
const Token = tokens.Token;
const TokenType = tokens.TokenType;

pub const InterpreterError = error{
    OutOfMemory,
    TypeError,
    UndefinedVariable,
    UnsupportedOperator,
};

pub const Interpreter = struct {
    env: Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Interpreter {
        return .{
            .env = Environment.init(null, allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.env.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []const ast.Statement) !void {
        for (statements) |statement| {
            switch (statement) {
                .var_declaration => |decl| try self.declare(decl.name, decl.initializer),
                .expression => |expr| _ = try self.evaluate(expr),
            }
        }
    }

    pub fn declare(self: *Interpreter, token: Token, initializer: ?Expression) !void {
        const value = if (initializer) |init_expr| try self.evaluate(init_expr) else Value.null;
        try self.env.define(token, value);
    }

    pub fn evaluate(self: *Interpreter, expr: ast.Expression) InterpreterError!Value {
        switch (expr) {
            .literal => |e| return e.value,

            // Variables
            .variable => |e| return self.env.get(e.token),
            .var_assignment => |e| {
                const value = try self.evaluate(e.value);
                try self.env.assign(e.token, value);
                return value;
            },

            // Operations
            .unary => |e| {
                const right = try self.evaluate(e.right);
                switch (e.operator.type) {
                    .bang => return Value{ .boolean = !isTruthy(right) },
                    .minus => return Value{ .int = -(try right.asInt()) },
                    else => return InterpreterError.UnsupportedOperator,
                }
            },
            .binary => |e| {
                const left = try self.evaluate(e.left);
                const right = try self.evaluate(e.right);

                const left_val = try left.asInt();
                const right_val = try right.asInt();

                switch (e.operator.type) {
                    .plus => return Value{ .int = left_val + right_val },
                    .minus => return Value{ .int = left_val - right_val },
                    .star => return Value{ .int = left_val * right_val },
                    .slash => return Value{ .int = left_val / right_val },

                    .equal_equal => return Value{ .boolean = left_val == right_val },
                    .bang_equal => return Value{ .boolean = left_val != right_val },
                    .greater => return Value{ .boolean = left_val > right_val },
                    .greater_equal => return Value{ .boolean = left_val >= right_val },
                    .less => return Value{ .boolean = left_val < right_val },
                    .less_equal => return Value{ .boolean = left_val <= right_val },

                    else => return InterpreterError.UnsupportedOperator,
                }
            },
        }
    }
};

fn isTruthy(value: Value) bool {
    return switch (value) {
        .null => false,
        .boolean => value.boolean,
        .int => value.int != 0,
        .string => value.string.len > 0,
    };
}
