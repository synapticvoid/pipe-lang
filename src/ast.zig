const Token = @import("tokens.zig").Token;
const Callable = @import("callable.zig").Callable;

pub const Statement = union(enum) {
    // Expressions
    expression: Expression,

    // Declarations
    var_declaration: VarDeclaration,
    fn_declaration: FnDeclaration,

    // Control flow
    @"return": Return,

    // Errors
    // error Name { Variant1, Variant2 }
    error_declaration: ErrorDeclaration,

    // error Name = A | B
    error_union_declaration: ErrorUnionDeclaration,

    // -------------------------------------------------------------------

    pub const VarDeclaration = struct {
        name: Token,
        type_annotation: ?PipeTypeAnnotation,
        initializer: ?Expression,
        mutability: Mutability,
    };

    pub const FnDeclaration = struct {
        name: Token,
        params: []Param,
        return_type: ?PipeTypeAnnotation,
        body: []const Statement,
    };

    pub const Return = struct {
        // The 'return' keyword (for error reporting)
        token: Token,

        // null means `return;` (returns unit)
        value: ?Expression,
    };

    pub const ErrorDeclaration = struct {
        name: Token,
        variants: []const Token,
    };

    pub const ErrorUnionDeclaration = struct {
        name: Token,
        // Existing error type names being unioned
        members: []const Token,
    };
};

pub const Expression = union(enum) {
    // Literals
    literal: Literal,

    // Variables
    variable: Variable,
    var_assignment: *VarAssignment,

    // Functions
    fn_call: *FnCall,

    // Operations
    unary: *Unary,
    binary: *Binary,

    // Control flow
    if_expr: *If,
    block: *Block,

    // Errors
    try_expr: *Try,
    catch_expr: *Catch,

    // -------------------------------------------------------------------

    // Literals
    pub const Literal = struct {
        value: Value,
    };

    // Variables
    pub const Variable = struct {
        token: Token,
    };

    pub const VarAssignment = struct {
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

    // Errors
    pub const Try = struct {
        // 'try' keyword for error reporting
        token: Token,
        expression: Expression,
    };

    pub const Catch = struct {
        // 'catch' keyword for error reporting
        token: Token,

        // The faillible left-hand side
        expression: Expression,

        // The |e| capture, null if omitted
        binding: ?Token,
        handler: Expression,
    };
};

pub const Value = union(enum) {
    int: f64,
    string: []const u8,
    boolean: bool,
    function: Callable,
    null,
    unit,
    error_value: struct { message: []const u8 },

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
            .function => |f| switch (f) {
                .user => |u| try writer.print("fn<{s}>", .{u.declaration.name.lexeme}),
                .builtin => |b| try writer.print("fn<{s}>", .{b.name}),
            },
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
            .error_value => |e| try writer.print("error<{s}>", .{e.message}),
        }
    }
};

pub const Mutability = enum {
    mutable,
    constant,
};

pub const Param = struct {
    name: Token,
    type_annotation: PipeTypeAnnotation,
};

pub const PipeTypeAnnotation = union(enum) {
    // int, string, MyType
    named: Token,

    // !T
    inferred_error_union: *PipeTypeAnnotation,

    explicit_error_union: struct {
        // E in E!T
        error_set: Token,
        ok_type: *PipeTypeAnnotation,
    },
};
