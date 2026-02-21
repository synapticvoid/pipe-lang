const std = @import("std");

const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const types = @import("types.zig");
const PipeType = types.PipeType;
const Token = @import("tokens.zig").Token;

pub const TypeInfo = union(enum) {
    variable: VarSignature,
    function: FnSignature,
};

const VarSignature = struct {
    pipe_type: PipeType,
    mutability: ast.Mutability,
};

const FnSignature = struct {
    param_types: []const PipeType,
    return_type: PipeType,
};

const TypeCheckError = error{
    TypeMismatch,
    UndefinedVariable,
    UndefinedType,
    ConstReassignment,
    OutOfMemory,
};

pub const TypeEnvironment = struct {
    enclosing: ?*TypeEnvironment,
    symbols: std.StringHashMap(TypeInfo),
    allocator: std.mem.Allocator,

    pub fn init(enclosing: ?*TypeEnvironment, allocator: std.mem.Allocator) TypeEnvironment {
        return .{
            .enclosing = enclosing,
            .symbols = std.StringHashMap(TypeInfo).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn define(self: *TypeEnvironment, name: []const u8, info: TypeInfo) !void {
        try self.symbols.put(name, info);
    }

    pub fn get(self: *TypeEnvironment, name: []const u8) !TypeInfo {
        if (self.symbols.get(name)) |info| {
            return info;
        } else if (self.enclosing) |enclosing| {
            return enclosing.get(name);
        } else {
            return error.UndefinedVariable;
        }
    }
};

pub const TypeChecker = struct {
    env: *TypeEnvironment,
    //
    // Expected return type of the current function
    expected_return_type: ?PipeType = null,

    // True if we're in a fallible function
    in_fallible_fn: bool = false,

    // Lookup table to match error uses to error declarations
    // name -> error variant identifier
    error_sets: std.StringHashMap([]const []const u8),

    // Last type checker error message
    last_error: ?[]const u8 = null,

    allocator: std.mem.Allocator,

    // NOTE: -- Public API

    pub fn init(allocator: std.mem.Allocator) !TypeChecker {
        const env = try allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(null, allocator);
        try builtins.registerAllTypes(env);

        return .{
            .env = env,
            .error_sets = std.StringHashMap([]const []const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn check(self: *TypeChecker, statements: []const ast.Statement) TypeCheckError!void {
        _ = try self.checkBody(statements);
    }

    // NOTE: -- Statements

    fn checkBody(self: *TypeChecker, statements: []const ast.Statement) TypeCheckError!PipeType {
        var last_type: PipeType = .unit;
        for (statements) |statement| {
            switch (statement) {
                .expression => |expr| last_type = try expectType(try self.checkExpression(expr)),
                .var_declaration => |decl| {
                    try self.checkVarDeclarationStatement(decl);
                    last_type = .unit;
                },
                .fn_declaration => |decl| {
                    try self.checkFnDeclarationStatement(decl);
                    last_type = .unit;
                },
                .@"return" => |ret| last_type = try self.checkReturnStatement(ret),
                .error_declaration => |decl| try self.registerErrorSet(decl),
                .error_union_declaration => |decl| try self.registerErrorUnion(decl),
            }
        }

        return last_type;
    }

    fn checkVarDeclarationStatement(self: *TypeChecker, decl: ast.Statement.VarDeclaration) !void {
        const line = decl.name.line;
        var resolved_type: PipeType = undefined;

        if (decl.initializer) |init_expr| {
            const inferred = try expectType(try self.checkExpression(init_expr));

            if (decl.type_annotation) |annotation| {
                const annotated = try self.resolveTypeAnnotation(annotation);
                if (!inferred.compatible(annotated)) {
                    return self.fail(error.TypeMismatch, "Type mismatch: '{s}' declared as {s}, got {s} at line {d}", .{ decl.name.lexeme, @tagName(annotated), @tagName(inferred), line });
                } else {
                    resolved_type = inferred;
                }
            } else {
                resolved_type = inferred;
            }
        } else if (decl.type_annotation) |annotation| {
            resolved_type = try self.resolveTypeAnnotation(annotation);
        } else {
            return self.fail(error.TypeMismatch, "Type mismatch: '{s}' has no type annotation or initializer at line {d}", .{ decl.name.lexeme, line });
        }

        try self.env.define(decl.name.lexeme, .{ .variable = .{
            .pipe_type = resolved_type,
            .mutability = decl.mutability,
        } });
    }

    fn checkFnDeclarationStatement(self: *TypeChecker, decl: ast.Statement.FnDeclaration) !void {
        // Create environment
        var env = try self.allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(self.env, self.allocator);

        // Save parameter types to env
        const param_types = try self.allocator.alloc(PipeType, decl.params.len);

        for (decl.params, 0..) |param, i| {
            const param_type = try self.resolveTypeAnnotation(param.type_annotation);
            try env.define(param.name.lexeme, .{ .variable = .{
                .pipe_type = param_type,
                .mutability = .mutable,
            } });
            param_types[i] = param_type;
        }

        // Switch environments to check the function's body
        const previous = self.env;
        self.env = env;
        defer self.env = previous;

        // Resolve return type
        var return_type: PipeType = PipeType.unit;
        if (decl.return_type) |ret_type| {
            return_type = try self.resolveTypeAnnotation(ret_type);
        }

        // Determine is fallible for try/catch checks
        const previous_fallible = self.in_fallible_fn;
        self.in_fallible_fn = return_type.isFallible();
        defer self.in_fallible_fn = previous_fallible;

        // Save a restore the previous return type, we might be in a nested function
        const previous_return_type = self.expected_return_type;
        self.expected_return_type = return_type;
        defer self.expected_return_type = previous_return_type;

        // Check the body
        const body_type = try self.checkBody(decl.body);
        if (!body_type.compatible(return_type)) {
            return self.fail(error.TypeMismatch, "Type mismatch: function '{s}' should return {s}, got {s} at line {d}", .{ decl.name.lexeme, @tagName(return_type), @tagName(body_type), decl.name.line });
        }

        // Save the function signature to the *enclosing* environment
        try previous.define(decl.name.lexeme, .{ .function = .{
            .param_types = param_types,
            .return_type = return_type,
        } });
    }

    fn checkReturnStatement(self: *TypeChecker, ret: ast.Statement.Return) TypeCheckError!PipeType {
        const return_type: PipeType = if (ret.value) |value|
            try expectType(try self.checkExpression(value))
        else
            .unit;

        if (self.expected_return_type) |expected| {
            if (!return_type.compatible(expected)) {
                return self.fail(error.TypeMismatch, "Type mismatch: expected return type {s}, got {s} at line {d}", .{ @tagName(expected), @tagName(return_type), ret.token.line });
            }
        }

        return return_type;
    }

    // NOTE: -- Expressions

    fn checkExpression(self: *TypeChecker, expr: ast.Expression) TypeCheckError!TypeInfo {
        return switch (expr) {
            .literal => |lit| self.checkLiteral(lit),
            .variable => |v| self.checkVariable(v),
            .unary => |u| self.checkUnary(u),
            .binary => |b| self.checkBinary(b),
            .if_expr => |if_expr| self.checkIf(if_expr),
            .var_assignment => |assign| self.checkVarAssignment(assign),
            .fn_call => |call| self.checkFnCall(call),
            .try_expr => |try_expr| self.checkTry(try_expr),
            .catch_expr => |catch_expr| self.checkCatch(catch_expr),
            .block => |blk| {
                const block_type = try self.checkBody(blk.statements);
                return .{ .variable = .{ .pipe_type = block_type, .mutability = .constant } };
            },
        };
    }

    fn checkLiteral(_: *TypeChecker, literal: ast.Expression.Literal) !TypeInfo {
        const pipe_type: PipeType = switch (literal.value) {
            .boolean => .bool,
            .int => .int,
            .string => .string,
            .null, .unit, .error_value => .unit,
            .function => return error.TypeMismatch,
        };
        return .{ .variable = .{ .pipe_type = pipe_type, .mutability = .constant } };
    }

    fn checkVariable(self: *TypeChecker, v: ast.Expression.Variable) !TypeInfo {
        return self.env.get(v.token.lexeme);
    }

    fn checkUnary(self: *TypeChecker, unary: *ast.Expression.Unary) !TypeInfo {
        const right_type = try expectType(try self.checkExpression(unary.right));
        const op = unary.operator.lexeme;
        const line = unary.operator.line;

        const result: PipeType = switch (unary.operator.type) {
            .bang => {
                if (right_type != .bool) {
                    return self.fail(error.TypeMismatch, "Type mismatch: cannot apply '!' to {s} at line {d}", .{ @tagName(right_type), line });
                }
                return .{ .variable = .{ .pipe_type = .bool, .mutability = .constant } };
            },
            .minus => {
                if (!right_type.isNumeric()) {
                    return self.fail(error.TypeMismatch, "Type mismatch: cannot apply '-' to {s} at line {d}", .{ @tagName(right_type), line });
                }
                return .{ .variable = .{ .pipe_type = right_type, .mutability = .constant } };
            },
            else => return self.fail(error.TypeMismatch, "Type mismatch: unsupported unary operator '{s}' at line {d}", .{ op, line }),
        };

        return .{ .variable = .{ .pipe_type = result, .mutability = .constant } };
    }

    fn checkBinary(self: *TypeChecker, binary: *ast.Expression.Binary) !TypeInfo {
        const left_type = try expectType(try self.checkExpression(binary.left));
        const right_type = try expectType(try self.checkExpression(binary.right));
        const left_name = @tagName(left_type);
        const right_name = @tagName(right_type);
        const line = binary.operator.line;
        const op = binary.operator.lexeme;

        const op_err = "Type mismatch: cannot apply '{s}' to {s} and {s} at line {d}";

        const result: PipeType = switch (binary.operator.type) {
            // TODO: Handle float type
            .plus, .minus, .star, .slash => {
                if (!left_type.isNumeric() or !right_type.isNumeric()) {
                    return self.fail(error.TypeMismatch, op_err, .{ op, left_name, right_name, line });
                }
                return .{ .variable = .{ .pipe_type = .int, .mutability = .constant } };
            },
            .equal_equal, .bang_equal => {
                if (!left_type.compatible(right_type)) {
                    return self.fail(error.TypeMismatch, "Type mismatch: cannot compare {s} and {s} at line {d}", .{ left_name, right_name, line });
                }
                return .{ .variable = .{ .pipe_type = .bool, .mutability = .constant } };
            },
            .less, .less_equal, .greater, .greater_equal => {
                if (!left_type.isNumeric() or !right_type.isNumeric()) {
                    return self.fail(error.TypeMismatch, op_err, .{ op, left_name, right_name, line });
                }
                return .{ .variable = .{ .pipe_type = .bool, .mutability = .constant } };
            },
            else => return self.fail(error.TypeMismatch, "Type mismatch: unsupported operator '{s}' at line {d}", .{ op, line }),
        };

        return .{ .variable = .{ .pipe_type = result, .mutability = .constant } };
    }

    fn checkIf(self: *TypeChecker, if_expr: *ast.Expression.If) !TypeInfo {
        const condition_type = try expectType(try self.checkExpression(if_expr.condition));
        if (condition_type != .bool) {
            return self.fail(error.TypeMismatch, "Type mismatch: if condition must be Bool, got {s}", .{@tagName(condition_type)});
        }

        const then_type = try expectType(try self.checkExpression(if_expr.then_branch));
        if (if_expr.else_branch) |else_branch| {
            const else_type = try expectType(try self.checkExpression(else_branch));
            if (!then_type.compatible(else_type)) {
                return self.fail(error.TypeMismatch, "Type mismatch: if branches must have same type, got {s} and {s}", .{ @tagName(then_type), @tagName(else_type) });
            }
            return .{ .variable = .{ .pipe_type = then_type, .mutability = .constant } };
        }

        return .{ .variable = .{ .pipe_type = .unit, .mutability = .constant } };
    }

    fn checkVarAssignment(self: *TypeChecker, assign: *ast.Expression.VarAssignment) !TypeInfo {
        const info = try self.env.get(assign.token.lexeme);
        const v = switch (info) {
            .variable => |v_sig| v_sig,
            .function => return self.fail(error.TypeMismatch, "Type mismatch: cannot assign to function '{s}' at line {d}", .{ assign.token.lexeme, assign.token.line }),
        };

        if (v.mutability == .constant) {
            return self.fail(error.ConstReassignment, "Cannot reassign constant '{s}' at line {d}", .{ assign.token.lexeme, assign.token.line });
        }

        const value_type = try expectType(try self.checkExpression(assign.value));
        if (!v.pipe_type.compatible(value_type)) {
            return self.fail(error.TypeMismatch, "Type mismatch: expected {s}, got {s} at line {d}", .{
                @tagName(v.pipe_type),
                @tagName(value_type),
                assign.token.line,
            });
        }

        return .{ .variable = .{ .pipe_type = v.pipe_type, .mutability = v.mutability } };
    }

    fn checkFnCall(self: *TypeChecker, call: *ast.Expression.FnCall) !TypeInfo {
        const callee_info = try self.checkExpression(call.callee);
        const sig = switch (callee_info) {
            .function => |s| s,
            .variable => return self.fail(error.TypeMismatch, "Type mismatch: expression is not callable", .{}),
        };

        if (call.args.len != sig.param_types.len) {
            return self.fail(error.TypeMismatch, "Wrong number of arguments: expected {d}, got {d}", .{ sig.param_types.len, call.args.len });
        }

        for (call.args, sig.param_types, 0..) |arg, param_type, i| {
            const arg_type = try expectType(try self.checkExpression(arg));
            if (!arg_type.compatible(param_type)) {
                return self.fail(error.TypeMismatch, "Type mismatch: argument {d} expected {s}, got {s}", .{ i + 1, @tagName(param_type), @tagName(arg_type) });
            }
        }

        return .{ .variable = .{ .pipe_type = sig.return_type, .mutability = .constant } };
    }

    fn checkTry(self: *TypeChecker, try_expr: *ast.Expression.Try) !TypeInfo {
        const inner = try expectType(try self.checkExpression(try_expr.expression));
        if (!inner.isFallible()) {
            return self.fail(error.TypeMismatch, "Type mismatch: 'try' applied to non-fallible expression at line {d}", .{try_expr.token.line});
        }

        if (!self.in_fallible_fn) {
            return self.fail(error.TypeMismatch, "Type mismatch: 'try' used outside a fallible function at line {d}", .{try_expr.token.line});
        }

        // Unwrap and return the ok type
        return .{ .variable = .{ .pipe_type = inner.error_union.ok_type.*, .mutability = .constant } };
    }

    fn checkCatch(self: *TypeChecker, catch_expr: *ast.Expression.Catch) !TypeInfo {
        // Check left-hand side to be a fallible type
        const left = try expectType(try self.checkExpression(catch_expr.expression));
        if (!left.isFallible()) {
            return self.fail(error.TypeMismatch, "Type mismatch: 'catch' applied to non-fallible expression at line {d}", .{catch_expr.token.line});
        }

        // Dereference the ok type
        const ok_type = left.error_union.ok_type.*;

        // Used in the if, but Zig's defer fires in the block scope, not function scope
        // We need to restore the environment when checkCatch() returns
        const previous = self.env;
        defer self.env = previous;

        // If there's a binding |e|, introduce it in a new scope
        if (catch_expr.binding) |binding| {
            var inner_env = try self.allocator.create(TypeEnvironment);
            inner_env.* = TypeEnvironment.init(self.env, self.allocator);
            try inner_env.define(binding.lexeme, .{ .variable = .{ .pipe_type = .{
                .error_set = left.error_union.error_set orelse "Error",
            }, .mutability = .constant } });

            self.env = inner_env;
        }

        _ = try self.checkExpression(catch_expr.handler);

        return .{ .variable = .{ .pipe_type = ok_type, .mutability = .constant } };
    }

    // NOTE: -- Helpers

    // Record an error message and return the error.
    fn fail(self: *TypeChecker, err: TypeCheckError, comptime fmt: []const u8, args: anytype) TypeCheckError {
        self.last_error = std.fmt.allocPrint(self.allocator, fmt, args) catch null;
        return err;
    }

    // Unwrap a variable's type, or error if it's a function signature.
    fn expectType(info: TypeInfo) !PipeType {
        return switch (info) {
            .variable => |v| v.pipe_type,
            .function => error.TypeMismatch,
        };
    }

    // Look up a type name (e.g. "Int") and return the corresponding PipeType.
    fn resolveTypeName(name: []const u8) !PipeType {
        if (typeNames.get(name)) |pipe_type| {
            return pipe_type;
        } else {
            return error.UndefinedType;
        }
    }

    fn resolveTypeAnnotation(self: *TypeChecker, ann: ast.PipeTypeAnnotation) !PipeType {
        return switch (ann) {
            .named => |tok| try resolveTypeName(tok.lexeme),
            .inferred_error_union => |inner| {
                // Find the ok type
                const ok_type = try self.allocator.create(PipeType);
                ok_type.* = try self.resolveTypeAnnotation(inner.*);
                return .{ .error_union = .{
                    .error_set = null,
                    .ok_type = ok_type,
                } };
            },
            .explicit_error_union => |eu| {
                // An explicit type was used, but we don't have the error set
                if (!self.error_sets.contains(eu.error_set.lexeme)) {
                    return error.UndefinedType;
                }

                // Find the ok type
                const ok_type = try self.allocator.create(PipeType);
                ok_type.* = try self.resolveTypeAnnotation(eu.ok_type.*);
                return .{ .error_union = .{
                    .error_set = eu.error_set.lexeme,
                    .ok_type = ok_type,
                } };
            },
        };
    }

    // Register an error set in the error registry
    // error MyError = { Err1, Err2 } becomes { "MyError": ["Err1", "Err2"] }
    fn registerErrorSet(self: *TypeChecker, decl: ast.Statement.ErrorDeclaration) !void {
        const variants = try self.allocator.alloc([]const u8, decl.variants.len);
        for (decl.variants, 0..) |variant, i| {
            variants[i] = variant.lexeme;
        }
        try self.error_sets.put(decl.name.lexeme, variants);
    }

    // Register an error union in the error registry
    // error MyError = Err1 | Err2 becomes { "MyError": ["Err1", "Err2"] }
    fn registerErrorUnion(self: *TypeChecker, decl: ast.Statement.ErrorUnionDeclaration) !void {
        const members = try self.allocator.alloc([]const u8, decl.members.len);
        for (decl.members, 0..) |member, i| {
            members[i] = member.lexeme;
        }
        try self.error_sets.put(decl.name.lexeme, members);
    }
};

const typeNames = std.StaticStringMap(PipeType).initComptime(.{
    .{ "Bool", PipeType.bool },
    .{ "Float", PipeType.float },
    .{ "Int", PipeType.int },
    .{ "String", PipeType.string },
    .{ "Unit", PipeType.unit },
});
