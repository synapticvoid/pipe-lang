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
    VariableAlreadyDefined,
};

pub const Interpreter = struct {
    env: *Environment,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Interpreter {
        const env = try allocator.create(Environment);
        env.* = Environment.init(null, allocator);

        return .{
            .env = env,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.env.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []const ast.Statement) !void {
        for (statements) |statement| {
            try self.execute(statement);
        }
    }

    pub fn execute(self: *Interpreter, statement: ast.Statement) !void {
        switch (statement) {
            .var_declaration => |decl| try self.declare(decl.name, decl.initializer),
            .expression => |expr| _ = try self.evaluate(expr),
        }
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

            // Control flow
            .block => |e| {
                const env = try self.allocator.create(Environment);
                env.* = Environment.init(self.env, self.allocator);
                return try self.evaluateBlock(e, env);
            },

            .if_expr => |e| {
                const cond = try self.evaluate(e.condition);
                if (isTruthy(cond)) {
                    return try self.evaluate(e.then_branch);
                } else if (e.else_branch) |else_expr| {
                    return try self.evaluate(else_expr);
                }
                return Value.unit;
            },
        }
    }

    fn evaluateBlock(self: *Interpreter, block: *ast.Expression.Block, env: *Environment) !ast.Value {
        const previous = self.env;
        self.env = env;
        defer self.env = previous;

        if (block.statements.len == 0) {
            return Value.unit;
        }

        // Execute all statements except the last one
        for (block.statements[0 .. block.statements.len - 1]) |statement| {
            try self.execute(statement);
        }

        // We have to extract the value of the last statement
        const last = block.statements[block.statements.len - 1];
        return switch (last) {
            .expression => |expr| try self.evaluate(expr),
            else => {
                try self.execute(last);
                return Value.unit;
            },
        };
    }

    pub fn declare(self: *Interpreter, token: Token, initializer: ?Expression) !void {
        const value = if (initializer) |init_expr| try self.evaluate(init_expr) else Value.null;
        try self.env.define(token, value);
    }
};

fn isTruthy(value: Value) bool {
    return switch (value) {
        .null => false,
        .unit => false,
        .boolean => value.boolean,
        .int => value.int != 0,
        .string => value.string.len > 0,
    };
}
