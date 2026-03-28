const std = @import("std");
const activeTag = std.meta.activeTag;

const Token = @import("token.zig").Token;

pub const Statement = union(enum) {
    // Expressions
    expression: Expression,

    // Declarations
    var_declaration: VarDeclaration,
    fn_declaration: FnDeclaration,

    // Control flow
    @"return": Return,

    // Structs
    struct_declaration: StructDeclaration,
    enum_declaration: EnumDeclaration,

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

    pub const StructDeclaration = struct {
        name: Token,
        fields: []const FieldDeclaration,
        body_fields: []const FieldDeclaration,
        kind: StructKind,
        methods: []const FnDeclaration,
    };

    pub const EnumDeclaration = struct {
        name: Token,
        is_error: bool,
        variants: []const Variant,
    };

    pub const Variant = struct {
        name: Token,
        fields: []const FieldDeclaration,
    };

    pub const FieldDeclaration = struct {
        name: Token,
        type_annotation: PipeTypeAnnotation,
        mutability: Mutability,
        // null for constructor params, required for computed fields
        default_value: ?Expression,
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

    // Structs
    struct_init: *StructInit,
    field_access: *FieldAccess,
    field_assignment: *FieldAssignment,

    pub fn line(self: Expression) usize {
        return switch (self) {
            .literal => |l| l.token.line,
            .variable => |v| v.token.line,
            .var_assignment => |a| a.token.line,
            .fn_call => |c| c.callee.line(),
            .unary => |u| u.operator.line,
            .binary => |b| b.operator.line,
            .if_expr => |i| i.condition.line(),
            .block => |b| if (b.statements.len > 0) statementLine(b.statements[0]) else 0,
            .try_expr => |t| t.token.line,
            .catch_expr => |c| c.token.line,
            .struct_init => |s| s.name.line,
            .field_access => |f| f.name.line,
            .field_assignment => |f| f.name.line,
        };
    }

    // -------------------------------------------------------------------

    // Literals
    pub const Literal = struct {
        token: Token,
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

        // The fallible left-hand side
        expression: Expression,

        // The catch binding (e.g. `catch e { ... }`), null if omitted
        binding: ?Token,
        handler: Expression,
    };

    // Struct
    // const u = User("Bob");
    pub const StructInit = struct {
        name: Token,
        args: []const Expression,
    };

    // const n = u.name;
    pub const FieldAccess = struct {
        object: Expression,
        name: Token,
    };

    // u.name = "Alice";
    // object.name = value
    pub const FieldAssignment = struct {
        object: Expression,
        name: Token,
        value: Expression,
    };
};

pub const Value = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    null,
    unit,

    pub fn eql(self: Value, other: Value) bool {
        const self_tag = activeTag(self);
        const other_tag = activeTag(other);
        if (self_tag != other_tag) {
            return false;
        }

        return switch (self) {
            .int => |a| a == other.int,
            .string => |a| std.mem.eql(u8, a, other.string),
            .boolean => |a| a == other.boolean,
            .null, .unit => true,
        };
    }

    pub fn asInt(self: Value) !i64 {
        switch (self) {
            .int => |n| return n,
            else => return error.TypeError,
        }
    }

    // Returns true if the value is truthy
    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .null, .unit => false,
            .boolean => |b| b,
            .int => |n| n != 0,
            .string => |s| s.len > 0,
        };
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .int => true,
            else => false,
        };
    }

    pub fn format(self: Value, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .int => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
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

pub const StructKind = enum {
    plain,
    case,
};

pub fn statementLine(stmt: Statement) usize {
    return switch (stmt) {
        .expression => |e| e.line(),
        .var_declaration => |d| d.name.line,
        .fn_declaration => |d| d.name.line,
        .@"return" => |r| r.token.line,
        .struct_declaration => |d| d.name.line,
        .enum_declaration => |d| d.name.line,
    };
}

pub const PipeTypeAnnotation = union(enum) {
    named: Token, // e.g. `Int`, `MyEnum`
    inferred_error_union: *PipeTypeAnnotation, // e.g. `!Int` — error set inferred (not yet implemented)
    explicit_error_union: struct { // e.g. `MyError!Int`
        error_set: Token, // the E in E!T
        ok_type: *PipeTypeAnnotation,
    },
};
