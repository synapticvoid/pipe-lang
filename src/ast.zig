const std = @import("std");
const tokens = @import("tokens.zig");
const PipeFunction = @import("callable.zig").PipeFunction;
const Token = tokens.Token;

pub const Statement = union(enum) {
    // Expressions
    expression: Expression,

    // Declarations
    var_declaration: VarDeclaration,
    fn_declaration: FnDeclaration,

    pub const VarDeclaration = struct {
        name: Token,
        initializer: ?Expression,
    };

    pub const FnDeclaration = struct {
        name: Token,
        params: []const Token,
        body: []const Statement,
    };
};

pub const Expression = union(enum) {
    // Literals
    literal: Literal,

    // Variables
    variable: Variable,
    var_assignment: *VariableAssignment,

    // Functions
    fn_call: *FnCall,

    // Operations
    unary: *Unary,
    binary: *Binary,


    // Control flow
    if_expr: *If,
    block: *Block,

    // -------------------------------------------------------------------

    // Literals
    pub const Literal = struct {
        value: Value,
    };

    // Variables
    pub const Variable = struct {
        token: Token,
    };

    pub const VariableAssignment = struct {
        token: Token,
        value: Expression,
    };

    // Functions
    pub const FnCall = struct {
        callee: Expression,
        args: []const Expression,
    };

    // Control flow
    pub const If = struct {
        condition: Expression,
        then_branch: Expression,
        else_branch: ?Expression,
    };

    pub const Block = struct {
        statements: []const Statement,
    };

    // Operations
    pub const Unary = struct {
        operator: Token,
        right: Expression,
    };

    pub const Binary = struct {
        left: Expression,
        operator: Token,
        right: Expression,
    };
};

pub const Value = union(enum) {
    int: f64,
    string: []const u8,
    boolean: bool,
    function: PipeFunction,
    null,
    unit,

    pub fn asInt(self: Value) !f64 {
        switch (self) {
            .int => |n| return n,
            else => return error.TypeError,
        }
    }

    pub fn format(self: Value, writer: anytype) !void {
        switch (self) {
            .int => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .function => |f| try writer.print("fn<{s}>", .{f.declaration.name.lexeme}),
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
        }
    }
};

