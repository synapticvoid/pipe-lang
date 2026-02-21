const std = @import("std");

const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const types = @import("types.zig");
const PipeType = types.PipeType;
const Token = @import("tokens.zig").Token;

pub const TypeInfo = union(enum) {
    variable: VariableSignature,
    function: FunctionSignature,
};

const VariableSignature = struct {
    pipe_type: PipeType,
    mutability: ast.Mutability,
};

const FunctionSignature = struct {
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
    expected_return_type: ?PipeType = null,
    last_error: ?[]const u8 = null,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !TypeChecker {
        const env = try allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(null, allocator);
        try builtins.registerAllTypes(env);

        return .{ .env = env, .allocator = allocator };
    }

    pub fn check(self: *TypeChecker, statements: []const ast.Statement) TypeCheckError!void {
        _ = try self.checkBody(statements);
    }

    // --- Expression type checking ---

    fn checkExpression(self: *TypeChecker, expr: ast.Expression) TypeCheckError!TypeInfo {
        return switch (expr) {
            .literal => |lit| self.checkLiteral(lit),
            .variable => |v| self.checkVariable(v),
            .unary => |u| self.checkUnary(u),
            .binary => |b| self.checkBinary(b),
            .if_expr => |if_expr| self.checkIf(if_expr),
            .var_assignment => |assign| self.checkAssignment(assign),
            .fn_call => |call| self.checkFnCall(call),
            else => error.TypeMismatch,
        };
    }

    fn checkLiteral(_: *TypeChecker, literal: ast.Expression.Literal) !TypeInfo {
        const pipe_type: PipeType = switch (literal.value) {
            .boolean => .bool,
            .int => .int,
            .string => .string,
            .null, .unit => .unit,
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

    fn checkAssignment(self: *TypeChecker, assign: *ast.Expression.VariableAssignment) !TypeInfo {
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

    // --- Declaration type checking ---

    fn checkVarDeclaration(self: *TypeChecker, decl: ast.Statement.VarDeclaration) !void {
        const line = decl.name.line;
        var resolved_type: PipeType = undefined;

        if (decl.initializer) |init_expr| {
            const inferred = try expectType(try self.checkExpression(init_expr));

            if (decl.type_annotation) |annotation| {
                const annotated = try resolveTypeName(annotation.lexeme);
                if (!inferred.compatible(annotated)) {
                    return self.fail(error.TypeMismatch, "Type mismatch: '{s}' declared as {s}, got {s} at line {d}", .{ decl.name.lexeme, @tagName(annotated), @tagName(inferred), line });
                } else {
                    resolved_type = inferred;
                }
            } else {
                resolved_type = inferred;
            }
        } else if (decl.type_annotation) |annotation| {
            resolved_type = try resolveTypeName(annotation.lexeme);
        } else {
            return self.fail(error.TypeMismatch, "Type mismatch: '{s}' has no type annotation or initializer at line {d}", .{ decl.name.lexeme, line });
        }

        try self.env.define(decl.name.lexeme, .{ .variable = .{
            .pipe_type = resolved_type,
            .mutability = decl.mutability,
        } });
    }

    fn checkFunctionDeclaration(self: *TypeChecker, decl: ast.Statement.FnDeclaration) !void {
        // Create environment
        var env = try self.allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(self.env, self.allocator);

        // Save parameter types to env
        const param_types = try self.allocator.alloc(PipeType, decl.params.len);

        for (decl.params, 0..) |param, i| {
            const param_type = try resolveTypeName(param.type_annotation.lexeme);
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
        var return_type = PipeType.unit;
        if (decl.return_type) |ret_type| {
            return_type = try resolveTypeName(ret_type.lexeme);
        }

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

    fn checkBody(self: *TypeChecker, statements: []const ast.Statement) TypeCheckError!PipeType {
        var last_type: PipeType = .unit;
        for (statements) |statement| {
            switch (statement) {
                .expression => |expr| last_type = try expectType(try self.checkExpression(expr)),
                .var_declaration => |decl| {
                    try self.checkVarDeclaration(decl);
                    last_type = .unit;
                },
                .fn_declaration => |decl| {
                    try self.checkFunctionDeclaration(decl);
                    last_type = .unit;
                },
                .@"return" => |ret| last_type = try self.checkReturn(ret),
            }
        }

        return last_type;
    }

    fn checkReturn(self: *TypeChecker, ret: ast.Statement.Return) TypeCheckError!PipeType {
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

    // NOTE: --- Helpers ---

    fn fail(self: *TypeChecker, err: TypeCheckError, comptime fmt: []const u8, args: anytype) TypeCheckError {
        self.last_error = std.fmt.allocPrint(self.allocator, fmt, args) catch null;
        return err;
    }

    fn expectType(info: TypeInfo) !PipeType {
        return switch (info) {
            .variable => |v| v.pipe_type,
            .function => error.TypeMismatch,
        };
    }

    fn resolveTypeName(name: []const u8) !PipeType {
        if (typeNames.get(name)) |pipe_type| {
            return pipe_type;
        } else {
            return error.UndefinedType;
        }
    }
};

const typeNames = std.StaticStringMap(PipeType).initComptime(.{
    .{ "Bool", PipeType.bool },
    .{ "Float", PipeType.float },
    .{ "Int", PipeType.int },
    .{ "String", PipeType.string },
    .{ "Unit", PipeType.unit },
});
