const tokens = @import("tokens.zig");
const Token = tokens.Token;

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
};