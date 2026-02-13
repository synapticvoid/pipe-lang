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
            .binary => |e| {
                const left = try self.evaluate(e.left);
                const right = try self.evaluate(e.right);

                const left_val = try left.asNumber();
                const right_val = try right.asNumber();

                switch (e.operator.type) {
                    .plus => return Value { .number = left_val + right_val },
                    .minus => return Value { .number = left_val - right_val },
                    .star => return Value { .number = left_val * right_val },
                    .slash => return Value { .number = left_val / right_val },
                    else => return InterpreterError.UnsupportedOperator,
                }
            },
        }
    }
};
