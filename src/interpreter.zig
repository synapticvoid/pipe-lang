const ast = @import("ast.zig");
const tokens = @import("tokens.zig");

const Expression = ast.Expression;
const Value = ast.Value;
const TokenType = tokens.TokenType;

pub const InterpreterError = error{
    UnsupportedOperator,
    TypeError,
};

pub const Interpreter = struct {
    pub fn interpret(self: *Interpreter, statements: []const ast.Statement) !void {
        for (statements) |statement| {
            _ = try self.evaluate(statement.expression);
        }
    }

    pub fn evaluate(self: *Interpreter, expr: ast.Expression) InterpreterError!Value {
        switch (expr) {
            .literal => |e| return e.value,
            .unary => |e| {
                const right = try self.evaluate(e.right);
                switch (e.operator.type) {
                    .bang => return Value {.boolean = !isTruthy(right) },
                    .minus => return Value { .int = -(try right.asInt())},
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
