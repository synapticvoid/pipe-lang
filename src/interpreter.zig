const std = @import("std");
const ast = @import("ast.zig");
const tokens = @import("tokens.zig");

const Environment = @import("environment.zig").Environment;
const Callable = @import("callable.zig").Callable;
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
            .var_declaration => |decl| try self.declareVar(decl),
            .fn_declaration => |decl| try self.declareFn(decl),
            .expression => |expr| _ = try self.evaluate(expr),
        }
    }

    pub fn evaluate(self: *Interpreter, expr: ast.Expression) InterpreterError!Value {
        switch (expr) {
            .literal => |e| return e.value,

            // Variables
            .variable => |e| return self.env.get(e.token.lexeme),
            .var_assignment => |e| {
                const value = try self.evaluate(e.value);
                try self.env.assign(e.token.lexeme, value);
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
                return try self.evaluateBlock(e.statements, env);
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

            // Functions
            .fn_call => |e| {
                const function = try self.evaluate(e.callee);
                switch (function) {
                    .function => |callable| switch (callable) {
                        .user => |user_fn| return try self.callFunction(user_fn, e.args),
                        .builtin => |builtin_fn| {
                            var args: std.ArrayList(Value) = .{};
                            for (e.args) |arg| {
                                try args.append(self.allocator, try self.evaluate(arg));
                            }
                            return builtin_fn.func(args.items);
                        },
                    },
                    else => return InterpreterError.TypeError,
                }
            },
        }
    }

    fn evaluateBlock(self: *Interpreter, statements: []const ast.Statement, env: *Environment) !ast.Value {
        const previous = self.env;
        self.env = env;
        defer self.env = previous;

        if (statements.len == 0) {
            return Value.unit;
        }

        // Execute all statements except the last one
        for (statements[0 .. statements.len - 1]) |statement| {
            try self.execute(statement);
        }

        // We have to extract the value of the last statement
        const last = statements[statements.len - 1];
        return switch (last) {
            .expression => |expr| try self.evaluate(expr),
            else => {
                try self.execute(last);
                return Value.unit;
            },
        };
    }

    pub fn declareVar(self: *Interpreter, var_expr: ast.Statement.VarDeclaration) !void {
        const value = if (var_expr.initializer) |init_expr| try self.evaluate(init_expr) else Value.null;
        try self.env.define(var_expr.name.lexeme, value);
    }

    pub fn declareFn(self: *Interpreter, fn_expr: ast.Statement.FnDeclaration) !void {
        const user_fn = Callable.UserFn{ .closure = self.env, .declaration = fn_expr };
        try self.env.define(fn_expr.name.lexeme, .{ .function = .{ .user = user_fn } });
    }

    pub fn callFunction(self: *Interpreter, function: Callable.UserFn, args: []const Expression) !Value {
        // Create environment with params
        const env = try self.allocator.create(Environment);
        env.* = Environment.init(function.closure, self.allocator);
        for (args, function.declaration.params) |arg, param| {
            const value = try self.evaluate(arg);
            try env.define(param.lexeme, value);
        }

        // Execute function's body
        return try self.evaluateBlock(function.declaration.body, env);

    }
};

fn isTruthy(value: Value) bool {
    return switch (value) {
        .null => false,
        .unit => false,
        .boolean => value.boolean,
        .int => value.int != 0,
        .function => true,
        .string => value.string.len > 0,
    };
}
