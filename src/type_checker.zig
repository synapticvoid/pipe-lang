const std = @import("std");
const ast = @import("ast.zig");
const types = @import("types.zig");
const PipeType = types.PipeType;
const Token = @import("tokens.zig").Token;

const TypeInfo = union(enum) {
    variable: VariableSignature,
    function: FunctionSignature,
};

const VariableSignature = struct {
    pipe_type: PipeType,
    mutability: ast.Mutability,
};

const FunctionSignature = struct {
    param_types: []PipeType,
    return_type: PipeType,
};

const TypeCheckError = error{
    TypeMismatch,
    UndefinedVariable,
    UndefinedType,
    ConstReassignment,
    OutOfMemory,
};

const TypeEnvironment = struct {
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

    fn checkExpression(self: *TypeChecker, expr: ast.Expression) TypeCheckError!PipeType {
        return switch (expr) {
            .unary => |u| self.checkUnary(u),
            .binary => |u| self.checkBinary(u),
            .literal => |lit| self.checkLiteral(lit),
            .variable => |v| self.checkVariable(v),
            .if_expr => |if_expr| self.checkIf(if_expr),
            .var_assignment => |assign| self.checkAssignment(assign),
            else => error.TypeMismatch,
        };
    }

    fn checkIf(self: *TypeChecker, if_expr: *ast.Expression.If) !PipeType {
        const condition_type = try self.checkExpression(if_expr.condition);
        if (condition_type != .bool) {
            return error.TypeMismatch;
        }

        const then_type = try self.checkExpression(if_expr.then_branch);
        // If else expression exists, its type must match with then
        if (if_expr.else_branch) |else_branch| {
            const else_type = try self.checkExpression(else_branch);
            if (!then_type.compatible(else_type)) {
                return error.TypeMismatch;
            }
            return then_type;
        }

        return PipeType.unit;
    }

    fn checkFunctionDeclaration(self: *TypeChecker, decl: ast.Statement.FnDeclaration) !void {
        // Create function's environment
        var env = try self.allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(self.env, self.allocator);

        const param_types = try self.allocator.alloc(PipeType, decl.params.len);

        // Resolve type for each param
        for (decl.params, 0..) |param, i| {
            const param_type = try resolveTypeName(param.type_annotation.lexeme);
            try env.define(param.name.lexeme, .{ .variable = .{
                .pipe_type = param_type,
                .mutability = .mutable,
            } });

            param_types[i] = param_type;
        }

        // Swap current env for type checking
        const previous = self.env;
        self.env = env;
        defer self.env = previous;

        // Type check body statements
        try self.check(decl.body);

        // Resolve return type
        var return_type = PipeType.unit;
        if (decl.return_type) |ret_type| {
            return_type = try resolveTypeName(ret_type.lexeme);
        }

        // Define function in environment
        try previous.define(decl.name.lexeme, .{ .function = .{
            .param_types = param_types,
            .return_type = return_type,
        } });
    }

    fn checkUnary(self: *TypeChecker, unary: *ast.Expression.Unary) !PipeType {
        const right_type = try self.checkExpression(unary.right);
        return switch (unary.operator.type) {
            .bang => if (right_type == .bool) .bool else error.TypeMismatch,
            .minus => if (right_type.isNumeric()) right_type else error.TypeMismatch,
            else => error.TypeMismatch,
        };
    }

    fn checkBinary(self: *TypeChecker, binary: *ast.Expression.Binary) !PipeType {
        const left_type = try self.checkExpression(binary.left);
        const right_type = try self.checkExpression(binary.right);
        return switch (binary.operator.type) {
            // TODO: Handle float type
            .plus, .minus, .star, .slash => if (left_type.isNumeric() and right_type.isNumeric()) .int else error.TypeMismatch,
            .equal_equal, .bang_equal => if (left_type.compatible(right_type)) .bool else error.TypeMismatch,
            .less, .less_equal, .greater, .greater_equal => if (left_type.isNumeric() and right_type.isNumeric()) .bool else error.TypeMismatch,
            else => error.TypeMismatch,
        };
    }

    fn checkVariable(self: *TypeChecker, v: ast.Expression.Variable) !PipeType {
        const info = try self.env.get(v.token.lexeme);
        return switch (info) {
            .variable => |v_sig| v_sig.pipe_type,
            .function => error.TypeMismatch,
        };
    }

    fn checkAssignment(self: *TypeChecker, assign: *ast.Expression.VariableAssignment) !PipeType {
        const info = try self.env.get(assign.token.lexeme);
        const v = switch (info) {
            .variable => |v_sig| v_sig,
            .function => return error.TypeMismatch,
        };

        if (v.mutability == .constant) {
            return error.ConstReassignment;
        }

        const value_type = try self.checkExpression(assign.value);
        if (!v.pipe_type.compatible(value_type)) {
            return error.TypeMismatch;
        }

        return v.pipe_type;
    }

    fn checkVarDeclaration(self: *TypeChecker, decl: ast.Statement.VarDeclaration) !void {
        var resolved_type: PipeType = undefined;

        // 1. If initializer exists, infer its type
        if (decl.initializer) |init_expr| {
            const inferred = try self.checkExpression(init_expr);

            // 2. If annotation exists, resolve it
            if (decl.type_annotation) |annotation| {
                const annotated = try resolveTypeName(annotation.lexeme);

                // 3. If botch, check they match
                if (!inferred.compatible(annotated)) {
                    return error.TypeMismatch;
                } else {
                    resolved_type = inferred;
                }
            } else {
                resolved_type = inferred;
            }
        } else if (decl.type_annotation) |annotation| {
            // Annotation only: trust it
            resolved_type = try resolveTypeName(annotation.lexeme);
        } else {
            // Neither, can't determine type
            return error.TypeMismatch;
        }

        // 4. Define in environment with the resolved type + decl.mutability
        try self.env.define(decl.name.lexeme, .{ .variable = .{
            .pipe_type = resolved_type,
            .mutability = decl.mutability,
        } });
    }

    fn checkLiteral(_: *TypeChecker, literal: ast.Expression.Literal) !PipeType {
        return switch (literal.value) {
            .boolean => PipeType.bool,
            .int => PipeType.int,
            .string => PipeType.string,
            .null, .unit => PipeType.unit,
            .function => error.TypeMismatch,
        };
    }

    fn resolveTypeName(name: []const u8) !PipeType {
        if (typeNames.get(name)) |pipe_type| {
            return pipe_type;
        } else {
            // TODO handle custom types?
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
