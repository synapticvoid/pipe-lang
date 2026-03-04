const std = @import("std");
const activeTag = std.meta.activeTag;

const Token = @import("token.zig").Token;
const Callable = @import("callable.zig").Callable;

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
    function: Callable,
    null,
    unit,

    // pointer so that copies of a value share identity (u2 = u1 means u1.field == u2.field)
    // var u1 = User("Bob");
    // var u2 = u1;
    // if (u1.name == u2.name) { ... } // true
    struct_instance: *StructInstance,

    pub const StructInstance = struct {
        type_name: []const u8,
        field_names: []const []const u8,
        body_field_values: []Value,
        body_field_names: []const []const u8,
        field_values: []Value,
        kind: StructKind,
    };

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
            .struct_instance => |a| {
                const b = other.struct_instance;
                if (a.kind == .plain) {
                    return a == b;
                }
                if (!std.mem.eql(u8, a.type_name, b.type_name)) {
                    return false;
                }
                for (a.field_values, b.field_values) |av, bv| {
                    if (!av.eql(bv)) {
                        return false;
                    }
                }
                return true;
            },
            else => false,
        };
    }

    pub fn asInt(self: Value) !i64 {
        switch (self) {
            .int => |n| return n,
            else => return error.TypeError,
        }
    }

    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .null, .unit => false,
            .boolean => |b| b,
            .int => |n| n != 0,
            .function => true,
            .string => |s| s.len > 0,
            .struct_instance => |_| true,
        };
    }

    pub fn format(self: Value, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .int => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .function => |f| switch (f) {
                .user => |u| try writer.print("fn<{s}>", .{u.declaration.name.lexeme}),
                .builtin => |bi| try writer.print("fn<{s}>", .{bi.name}),
                .struct_constructor => |sc| try writer.print("fn<{s}>", .{sc.name}),
            },
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
            .struct_instance => |si_ptr| {
                const si = si_ptr.*;
                switch (si.kind) {
                    .plain => try writer.print("<{s}>", .{si.type_name}),
                    .case => {
                        try writer.print("{s}(", .{si.type_name});
                        for (si.field_names, si.field_values, 0..) |name, value, i| {
                            if (i > 0) {
                                try writer.writeAll(", ");
                            }
                            try writer.print("{s}=", .{name});
                            try value.format(writer);
                        }
                        try writer.writeAll(")");
                    },
                }
            },
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

pub const PipeTypeAnnotation = union(enum) {
    named: Token, // e.g. `Int`, `MyEnum`
    inferred_error_union: *PipeTypeAnnotation, // e.g. `!Int` — error set inferred (not yet implemented)
    explicit_error_union: struct { // e.g. `MyError!Int`
        error_set: Token, // the E in E!T
        ok_type: *PipeTypeAnnotation,
    },
};
