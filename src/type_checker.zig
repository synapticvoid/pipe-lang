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
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !TypeChecker {
        const env = try allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(null, allocator);
        try builtins.registerAllTypes(env);

        return .{ .env = env, .allocator = allocator };
    }

    pub fn check(self: *TypeChecker, statements: []const ast.Statement) TypeCheckError!void {
        for (statements) |statement| {
            switch (statement) {
                .expression => |expr| _ = try self.checkExpression(expr),
                .var_declaration => |decl| try self.checkVarDeclaration(decl),
                .fn_declaration => |decl| try self.checkFunctionDeclaration(decl),
            }
        }
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
        const result: PipeType = switch (unary.operator.type) {
            .bang => if (right_type == .bool) .bool else return error.TypeMismatch,
            .minus => if (right_type.isNumeric()) right_type else return error.TypeMismatch,
            else => return error.TypeMismatch,
        };
        return .{ .variable = .{ .pipe_type = result, .mutability = .constant } };
    }

    fn checkBinary(self: *TypeChecker, binary: *ast.Expression.Binary) !TypeInfo {
        const left_type = try expectType(try self.checkExpression(binary.left));
        const right_type = try expectType(try self.checkExpression(binary.right));
        const result: PipeType = switch (binary.operator.type) {
            // TODO: Handle float type
            .plus, .minus, .star, .slash => if (left_type.isNumeric() and right_type.isNumeric()) .int else return error.TypeMismatch,
            .equal_equal, .bang_equal => if (left_type.compatible(right_type)) .bool else return error.TypeMismatch,
            .less, .less_equal, .greater, .greater_equal => if (left_type.isNumeric() and right_type.isNumeric()) .bool else return error.TypeMismatch,
            else => return error.TypeMismatch,
        };
        return .{ .variable = .{ .pipe_type = result, .mutability = .constant } };
    }

    fn checkIf(self: *TypeChecker, if_expr: *ast.Expression.If) !TypeInfo {
        const condition_type = try expectType(try self.checkExpression(if_expr.condition));
        if (condition_type != .bool) {
            return error.TypeMismatch;
        }

        const then_type = try expectType(try self.checkExpression(if_expr.then_branch));
        if (if_expr.else_branch) |else_branch| {
            const else_type = try expectType(try self.checkExpression(else_branch));
            if (!then_type.compatible(else_type)) {
                return error.TypeMismatch;
            }
            return .{ .variable = .{ .pipe_type = then_type, .mutability = .constant } };
        }

        return .{ .variable = .{ .pipe_type = .unit, .mutability = .constant } };
    }

    fn checkAssignment(self: *TypeChecker, assign: *ast.Expression.VariableAssignment) !TypeInfo {
        const info = try self.env.get(assign.token.lexeme);
        const v = switch (info) {
            .variable => |v_sig| v_sig,
            .function => return error.TypeMismatch,
        };

        if (v.mutability == .constant) {
            return error.ConstReassignment;
        }

        const value_type = try expectType(try self.checkExpression(assign.value));
        if (!v.pipe_type.compatible(value_type)) {
            return error.TypeMismatch;
        }

        return .{ .variable = .{ .pipe_type = v.pipe_type, .mutability = v.mutability } };
    }

    fn checkFnCall(self: *TypeChecker, call: *ast.Expression.FnCall) !TypeInfo {
        const callee_info = try self.checkExpression(call.callee);
        const sig = switch (callee_info) {
            .function => |s| s,
            .variable => return error.TypeMismatch,
        };

        if (call.args.len != sig.param_types.len) {
            return error.TypeMismatch;
        }

        for (call.args, sig.param_types) |arg, param_type| {
            const arg_type = try expectType(try self.checkExpression(arg));
            if (!arg_type.compatible(param_type)) {
                return error.TypeMismatch;
            }
        }

        return .{ .variable = .{ .pipe_type = sig.return_type, .mutability = .constant } };
    }

    // --- Declaration type checking ---

    fn checkVarDeclaration(self: *TypeChecker, decl: ast.Statement.VarDeclaration) !void {
        var resolved_type: PipeType = undefined;

        if (decl.initializer) |init_expr| {
            const inferred = try expectType(try self.checkExpression(init_expr));

            if (decl.type_annotation) |annotation| {
                const annotated = try resolveTypeName(annotation.lexeme);
                if (!inferred.compatible(annotated)) {
                    return error.TypeMismatch;
                } else {
                    resolved_type = inferred;
                }
            } else {
                resolved_type = inferred;
            }
        } else if (decl.type_annotation) |annotation| {
            resolved_type = try resolveTypeName(annotation.lexeme);
        } else {
            return error.TypeMismatch;
        }

        try self.env.define(decl.name.lexeme, .{ .variable = .{
            .pipe_type = resolved_type,
            .mutability = decl.mutability,
        } });
    }

    fn checkFunctionDeclaration(self: *TypeChecker, decl: ast.Statement.FnDeclaration) !void {
        var env = try self.allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(self.env, self.allocator);

        const param_types = try self.allocator.alloc(PipeType, decl.params.len);

        for (decl.params, 0..) |param, i| {
            const param_type = try resolveTypeName(param.type_annotation.lexeme);
            try env.define(param.name.lexeme, .{ .variable = .{
                .pipe_type = param_type,
                .mutability = .mutable,
            } });
            param_types[i] = param_type;
        }

        const previous = self.env;
        self.env = env;
        defer self.env = previous;

        try self.check(decl.body);

        var return_type = PipeType.unit;
        if (decl.return_type) |ret_type| {
            return_type = try resolveTypeName(ret_type.lexeme);
        }

        try previous.define(decl.name.lexeme, .{ .function = .{
            .param_types = param_types,
            .return_type = return_type,
        } });
    }

    // --- Helpers ---

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
});
