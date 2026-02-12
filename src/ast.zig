const tokens = @import("tokens.zig");
const Token = tokens.Token;

pub const ASTNode = union(enum) {
    expression: Expression,
};

pub const Expression = union(enum) {
    literal: Literal,
    binary: *Binary, // * to break the cycle of a recursive definition

    pub const Literal = struct {
        value: Value,
    };

    pub const Binary = struct {
        left: Expression,
        operator: Token,
        right: Expression,
    };
};

pub const Value = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
    null,

    pub fn asNumber(self: Value) !f64 {
        switch (self) {
            .number => |n| return n,
            else => return error.TypeError,
        }
    }
};

