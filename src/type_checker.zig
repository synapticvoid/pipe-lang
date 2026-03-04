const std = @import("std");

const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const types = @import("types.zig");
const utils = @import("utils.zig");
const PipeType = types.PipeType;
const Token = @import("token.zig").Token;

pub const SymbolInfo = union(enum) {
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
    NotImplemented,
    TypeMismatch,
    UndefinedField,
    UndefinedVariable,
    UndefinedType,
    ConstReassignment,
    UnconsumedFallible,
    OutOfMemory,
};

pub const TypeEnvironment = struct {
    enclosing: ?*TypeEnvironment,
    symbols: std.StringHashMap(SymbolInfo),
    allocator: std.mem.Allocator,

    pub fn init(enclosing: ?*TypeEnvironment, allocator: std.mem.Allocator) TypeEnvironment {
        return .{
            .enclosing = enclosing,
            .symbols = std.StringHashMap(SymbolInfo).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn define(self: *TypeEnvironment, name: []const u8, info: SymbolInfo) !void {
        try self.symbols.put(name, info);
    }

    pub fn get(self: *TypeEnvironment, name: []const u8) !SymbolInfo {
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

    // Current strut type - used to resolve Self in methods
    current_struct_type: ?[]const u8 = null,

    // Lookup table to match error uses to error declarations
    // name -> error variant identifier
    error_sets: std.StringHashMap([]const []const u8),

    // Lookup table to match struct types
    // name -> struct/union type
    type_registry: std.StringHashMap(types.TypeDef),

    // Last type checker error message
    last_error: ?[]const u8 = null,

    // Tracks unconsumed !T bindinds by name
    pending_fallibles: std.StringHashMap(void),

    allocator: std.mem.Allocator,

    // NOTE: -- Public API

    pub fn init(allocator: std.mem.Allocator) !TypeChecker {
        const env = try allocator.create(TypeEnvironment);
        env.* = TypeEnvironment.init(null, allocator);
        try builtins.registerAllTypes(env);

        return .{
            .env = env,
            .error_sets = std.StringHashMap([]const []const u8).init(allocator),
            .type_registry = std.StringHashMap(types.TypeDef).init(allocator),
            .pending_fallibles = std.StringHashMap(void).init(allocator),
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
                .expression => |expr| {
                    const expr_type = try asPipeType(try self.checkExpression(expr));
                    // Reject discarded !T results
                    // If the expression is an assignment, we should track it
                    if (self.isFallible(expr_type) and expr != .var_assignment) {
                        return self.fail(error.UnconsumedFallible, "Unconsumed fallible expression", .{});
                    }

                    last_type = expr_type;
                },
                .var_declaration => |decl| {
                    try self.checkVarDeclarationStatement(decl);
                    last_type = .unit;
                },
                .fn_declaration => |decl| {
                    try self.checkFnDeclarationStatement(decl);
                    last_type = .unit;
                },
                .@"return" => |ret| last_type = try self.checkReturnStatement(ret),
                .struct_declaration => |decl| try self.checkStructDeclarationStatement(decl),
                .enum_declaration => |decl| try self.checkEnumDeclarationStatement(decl),
            }
        }

        if (self.pending_fallibles.count() > 0) {
            return self.fail(error.UnconsumedFallible, "Unconsumed fallible bindings at scope exit", .{});
        }

        return last_type;
    }

    fn checkVarDeclarationStatement(self: *TypeChecker, decl: ast.Statement.VarDeclaration) !void {
        const line = decl.name.line;
        var resolved_type: PipeType = undefined;

        if (decl.initializer) |init_expr| {
            const inferred = try asPipeType(try self.checkExpression(init_expr));

            if (decl.type_annotation) |annotation| {
                const annotated = try self.resolveTypeAnnotation(annotation);
                if (!inferred.compatible(annotated) and !self.isNestedEnumCoercible(inferred, annotated)) {
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

        if (self.isFallible(resolved_type)) {
            // Consume the source binding if it's a variable
            if (decl.initializer) |init_expr| {
                if (init_expr == .variable) {
                    _ = self.pending_fallibles.remove(init_expr.variable.token.lexeme);
                }
            }
            //
            // If the resolved_type is !T, track it to check
            // if it's consumed on scope exit
            if (!std.mem.eql(u8, decl.name.lexeme, "_")) {
                try self.pending_fallibles.put(decl.name.lexeme, {});
            }
        }

        try self.env.define(decl.name.lexeme, .{ .variable = .{
            .pipe_type = resolved_type,
            .mutability = decl.mutability,
        } });
    }

    fn checkFnDeclarationStatement(self: *TypeChecker, decl: ast.Statement.FnDeclaration) !void {
        // Create environment
        const env = try self.allocator.create(TypeEnvironment);
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
        self.in_fallible_fn = self.isFallible(return_type);
        defer self.in_fallible_fn = previous_fallible;

        // Save a restore the previous return type, we might be in a nested function
        const previous_return_type = self.expected_return_type;
        self.expected_return_type = return_type;
        defer self.expected_return_type = previous_return_type;

        // Register the function signature in the enclosing environment *before*
        // checking the body so recursive calls can resolve the function.
        try previous.define(decl.name.lexeme, .{ .function = .{
            .param_types = param_types,
            .return_type = return_type,
        } });

        // Save and reset pending fallibles for this scope
        const previous_pending = self.pending_fallibles;
        self.pending_fallibles = std.StringHashMap(void).init(self.allocator);
        defer self.pending_fallibles = previous_pending;

        // Check the body
        const body_type = try self.checkBody(decl.body);
        if (!self.isCompatible(body_type, return_type)) {
            return self.fail(error.TypeMismatch, "Type mismatch: function '{s}' should return {s}, got {s} at line {d}", .{ decl.name.lexeme, @tagName(return_type), @tagName(body_type), decl.name.line });
        }
    }

    fn checkReturnStatement(self: *TypeChecker, ret: ast.Statement.Return) TypeCheckError!PipeType {
        const return_type: PipeType = if (ret.value) |value|
            try asPipeType(try self.checkExpression(value))
        else
            .unit;

        // Consume the pending fallible if returning a variable
        if (ret.value) |value| {
            if (value == .variable) {
                _ = self.pending_fallibles.remove(value.variable.token.lexeme);
            }
        }

        // Check remaining pending fallibles
        if (self.pending_fallibles.count() > 0) {
            return self.fail(error.UnconsumedFallible, "Unconsumed fallible bindings at return", .{});
        }

        if (self.expected_return_type) |expected| {
            if (!self.isCompatible(return_type, expected)) {
                return self.fail(error.TypeMismatch, "Type mismatch: expected return type {s}, got {s} at line {d}", .{ @tagName(expected), @tagName(return_type), ret.token.line });
            }
        }

        return return_type;
    }

    fn checkStructDeclarationStatement(self: *TypeChecker, decl: ast.Statement.StructDeclaration) !void {
        // Check fields info
        const resolved_fields = try self.resolveFields(decl.fields);

        // Declare the struct in the registry
        try self.type_registry.put(decl.name.lexeme, .{ .struct_type = .{
            .fields = resolved_fields,
            .kind = decl.kind,
        } });

        // Register constructor as a function
        try self.registerConstructor(decl.name.lexeme, resolved_fields, .{ .struct_type = decl.name.lexeme });

        // Register methods

        // 1. Set current struct name to have "Self" be resolved
        const previous = self.current_struct_type;
        self.current_struct_type = decl.name.lexeme;
        defer self.current_struct_type = previous;

        // 2. Register each method as Type.method
        for (decl.methods) |method| {
            // This checks and register the method to the TypeEnvironment as "method"
            // What we want is to register it as "Type.method"
            try self.checkFnDeclarationStatement(method);

            // Register the method as "Type.method" and remove "method"
            const qualified = try utils.memberName(self.allocator, decl.name.lexeme, method.name.lexeme);
            const sig = switch (try self.env.get(method.name.lexeme)) {
                .function => |s| s,
                else => unreachable,
            };

            // Instance method: first param is Self - strip if from the registered signature
            const registered_params = if (sig.param_types.len > 0 and sig.param_types[0].compatible(.{ .struct_type = decl.name.lexeme }))
                sig.param_types[1..]
            else
                sig.param_types;

            try self.env.define(qualified, .{ .function = .{
                .param_types = registered_params,
                .return_type = sig.return_type,
            } });
            _ = self.env.symbols.remove(method.name.lexeme);
        }
    }

    fn checkEnumDeclarationStatement(self: *TypeChecker, decl: ast.Statement.EnumDeclaration) !void {
        var resolved_variants = try self.allocator.alloc(types.VariantTypeInfo, decl.variants.len);
        for (decl.variants, 0..) |variant, i| {
            // If this is a zero-field variant whose name matches an existing enum,
            // treat it as a nested enum: synthesize a single field wrapping the inner type.
            const fields: []types.FieldInfo = blk: {
                if (variant.fields.len != 0) break :blk null;
                const info = self.type_registry.get(variant.name.lexeme) orelse break :blk null;
                switch (info) {
                    .enum_type => {
                        // An error enum must have only error variants
                        if (decl.is_error and !info.enum_type.is_error) {
                            return self.fail(error.TypeMismatch, "Type mismatch: enum '{s}' is not an error enum", .{variant.name.lexeme});
                        }

                        const synthetic = try self.allocator.alloc(types.FieldInfo, 1);
                        synthetic[0] = .{
                            .name = variant.name.lexeme,
                            .pipe_type = .{ .enum_type = variant.name.lexeme },
                            .mutability = .constant,
                        };
                        break :blk synthetic;
                    },
                    else => break :blk null,
                }
            } orelse try self.resolveFields(variant.fields);

            const variant_name = try utils.memberName(self.allocator, decl.name.lexeme, variant.name.lexeme);
            try self.registerConstructor(variant_name, fields, .{ .enum_type = decl.name.lexeme });

            resolved_variants[i] = .{
                .name = variant.name.lexeme,
                .fields = fields,
            };
        }

        try self.type_registry.put(decl.name.lexeme, .{ .enum_type = .{
            .is_error = decl.is_error,
            .is_result = false,
            .variants = resolved_variants,
        } });
    }

    // NOTE: -- Expressions

    fn checkExpression(self: *TypeChecker, expr: ast.Expression) TypeCheckError!SymbolInfo {
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
            .struct_init => |in| try self.checkInit(in),
            .field_access => |fa| self.checkFieldAccess(fa),
        };
    }

    fn checkLiteral(_: *TypeChecker, literal: ast.Expression.Literal) !SymbolInfo {
        const pipe_type: PipeType = switch (literal.value) {
            .boolean => .bool,
            .int => .int,
            .string => .string,
            .null, .unit => .unit,
            .function, .struct_instance => return error.TypeMismatch,
        };
        return .{ .variable = .{ .pipe_type = pipe_type, .mutability = .constant } };
    }

    fn checkVariable(self: *TypeChecker, v: ast.Expression.Variable) !SymbolInfo {
        return self.env.get(v.token.lexeme);
    }

    fn checkUnary(self: *TypeChecker, unary: *ast.Expression.Unary) !SymbolInfo {
        const right_type = try asPipeType(try self.checkExpression(unary.right));
        const op = unary.operator.lexeme;
        const line = unary.operator.line;

        _ = try self.expectUnwrapped(right_type, line);

        return switch (unary.operator.type) {
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
    }

    fn checkBinary(self: *TypeChecker, binary: *ast.Expression.Binary) !SymbolInfo {
        const left_type = try asPipeType(try self.checkExpression(binary.left));
        const right_type = try asPipeType(try self.checkExpression(binary.right));
        const left_name = @tagName(left_type);
        const right_name = @tagName(right_type);
        const line = binary.operator.line;
        const op = binary.operator.lexeme;

        // Guard clauses to ensure that fallible types are unwrapped
        _ = try self.expectUnwrapped(left_type, line);
        _ = try self.expectUnwrapped(right_type, line);

        const op_err = "Type mismatch: cannot apply '{s}' to {s} and {s} at line {d}";

        return switch (binary.operator.type) {
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
    }

    // Branch-level pending fallible tracking.
    //
    // Both branches of an if/else must consume the same !T bindings.
    // An if without else must not consume any, since the "no-else" path
    // leaves them unconsumed.
    //
    // Example — partial consumption is rejected:
    //
    //   const result = fallible();       // pending = { "result" }
    //   if (cond) { try result; }        // then consumes "result" → pending = {}
    //                                    // no else → else pending = { "result" }
    //                                    // mismatch → ERROR
    //
    // Example — both branches consume → OK:
    //
    //   const result = fallible();       // pending = { "result" }
    //   if (cond) { try result; }        // then pending = {}
    //   else      { try result; }        // else pending = {}
    //                                    // match → pending = {}
    //
    fn checkIf(self: *TypeChecker, if_expr: *ast.Expression.If) !SymbolInfo {
        const condition_type = try asPipeType(try self.checkExpression(if_expr.condition));
        if (condition_type != .bool) {
            return self.fail(error.TypeMismatch, "Type mismatch: if condition must be Bool, got {s}", .{@tagName(condition_type)});
        }

        // Snapshot pending set before either branch runs
        const pending_snapshot = try self.pending_fallibles.clone();

        // Check then-branch — may mutate pending_fallibles via try/catch
        const then_type = try asPipeType(try self.checkExpression(if_expr.then_branch));
        var return_type: PipeType = .unit;

        const after_then_pending = try self.pending_fallibles.clone();

        // Default: no else means else-branch leaves pending unchanged (= snapshot)
        var after_else_pending = pending_snapshot;

        // Restore snapshot so else-branch starts from the same baseline
        self.pending_fallibles = pending_snapshot;
        if (if_expr.else_branch) |else_branch| {
            const else_type = try asPipeType(try self.checkExpression(else_branch));
            if (!then_type.compatible(else_type)) {
                return self.fail(error.TypeMismatch, "Type mismatch: if branches must have same type, got {s} and {s}", .{ @tagName(then_type), @tagName(else_type) });
            }

            after_else_pending = try self.pending_fallibles.clone();

            return_type = then_type;
        }

        // Both branches must leave the same pending set
        if (!pendingSetsEqual(after_then_pending, after_else_pending)) {
            return self.fail(error.UnconsumedFallible, "if branches must consume the same fallible bindings", .{});
        }

        // Apply the merged result
        self.pending_fallibles = after_then_pending;

        return .{ .variable = .{ .pipe_type = return_type, .mutability = .constant } };
    }

    fn checkVarAssignment(self: *TypeChecker, assign: *ast.Expression.VarAssignment) !SymbolInfo {
        const info = try self.env.get(assign.token.lexeme);
        const v = switch (info) {
            .variable => |v_sig| v_sig,
            .function => return self.fail(error.TypeMismatch, "Type mismatch: cannot assign to function '{s}' at line {d}", .{ assign.token.lexeme, assign.token.line }),
        };

        if (v.mutability == .constant) {
            return self.fail(error.ConstReassignment, "Cannot reassign constant '{s}' at line {d}", .{ assign.token.lexeme, assign.token.line });
        }

        // Protect against the following scenario:
        // var r = foo(); // foo returns !T
        // r = bar();     // bar returns T
        // error in r is lost
        if (self.pending_fallibles.contains(assign.token.lexeme)) {
            return self.fail(error.UnconsumedFallible, "Reassignment of unconsumed fallible binding '{s}' at line {d}", .{ assign.token.lexeme, assign.token.line });
        }

        const value_type = try asPipeType(try self.checkExpression(assign.value));
        if (!v.pipe_type.compatible(value_type)) {
            return self.fail(error.TypeMismatch, "Type mismatch: expected {s}, got {s} at line {d}", .{
                @tagName(v.pipe_type),
                @tagName(value_type),
                assign.token.line,
            });
        }

        // Track the new assigned fallible binding
        if (self.isFallible(value_type)) {
            try self.pending_fallibles.put(assign.token.lexeme, {});
        }

        return .{ .variable = .{ .pipe_type = v.pipe_type, .mutability = v.mutability } };
    }

    fn checkFnCall(self: *TypeChecker, call: *ast.Expression.FnCall) !SymbolInfo {
        const callee_info = try self.checkExpression(call.callee);
        const sig = switch (callee_info) {
            .function => |s| s,
            .variable => return self.fail(error.TypeMismatch, "Type mismatch: expression is not callable", .{}),
        };

        if (call.args.len != sig.param_types.len) {
            return self.fail(error.TypeMismatch, "Wrong number of arguments: expected {d}, got {d}", .{ sig.param_types.len, call.args.len });
        }

        const line: usize = switch (call.callee) {
            .variable => |v| v.token.line,
            else => 0,
        };
        for (call.args, sig.param_types, 0..) |arg, param_type, i| {
            const arg_type = try asPipeType(try self.checkExpression(arg));

            // Fallible argument handling:
            //   foo(result)  where foo(x: Int)   → reject (!T where T expected)
            //   foo(result)  where foo(x: E!Int) → allow, consume "result" from pending
            if (self.isFallible(arg_type) and !self.isFallible(param_type)) {
                _ = try self.expectUnwrapped(arg_type, line);
            } else if (self.isFallible(arg_type) and arg == .variable) {
                // Consume the pending binding — obligation transfers to the callee
                _ = self.pending_fallibles.remove(arg.variable.token.lexeme);
            }

            if (!arg_type.compatible(param_type)) {
                return self.fail(error.TypeMismatch, "Type mismatch: argument {d} expected {s}, got {s}", .{ i + 1, @tagName(param_type), @tagName(arg_type) });
            }
        }

        return .{ .variable = .{ .pipe_type = sig.return_type, .mutability = .constant } };
    }

    fn checkInit(self: *TypeChecker, init_expr: *ast.Expression.StructInit) TypeCheckError!SymbolInfo {
        const info = self.type_registry.get(init_expr.name.lexeme) orelse return error.UndefinedType;

        const struct_type = switch (info) {
            .struct_type => |s| s,
            else => return error.TypeMismatch,
        };

        if (init_expr.args.len != struct_type.fields.len) {
            return self.fail(error.TypeMismatch, "Wrong number of arguments: expected {d}, got {d}", .{ struct_type.fields.len, init_expr.args.len });
        }

        // Compare args and field types
        for (init_expr.args, struct_type.fields, 0..) |arg, field, i| {
            const arg_type = try asPipeType(try self.checkExpression(arg));
            if (!arg_type.compatible(field.pipe_type)) {
                return self.fail(error.TypeMismatch, "Type mismatch: argument {d} expected {s}, got {s}", .{ i + 1, @tagName(field.pipe_type), @tagName(arg_type) });
            }
        }

        return .{ .variable = .{ .pipe_type = .{ .struct_type = init_expr.name.lexeme }, .mutability = .constant } };
    }

    fn checkFieldAccess(self: *TypeChecker, field_access: *ast.Expression.FieldAccess) TypeCheckError!SymbolInfo {
        // We have to check for a constructor call early on
        // Constructors are registered as function calls in the environment
        if (field_access.object == .variable) {
            const qualified = try utils.memberName(self.allocator, field_access.object.variable.token.lexeme, field_access.name.lexeme);

            if (self.env.get(qualified)) |val| {
                return val;
            } else |_| {}
        }

        const obj_type = try asPipeType(try self.checkExpression(field_access.object));

        const struct_name = switch (obj_type) {
            .struct_type => |name| name,
            .enum_type => |name| name,
            else => return error.TypeMismatch,
        };

        const info = self.type_registry.get(struct_name) orelse return error.UndefinedType;
        return switch (info) {
            .struct_type => |s| {
                // Look for field name in struct type
                for (s.fields) |field| {
                    if (std.mem.eql(u8, field.name, field_access.name.lexeme)) {
                        return .{ .variable = .{ .pipe_type = field.pipe_type, .mutability = field.mutability } };
                    }
                }

                // No field found - check for a method
                const qualified = try utils.memberName(self.allocator, struct_name, field_access.name.lexeme);
                if (self.env.get(qualified)) |method| {
                    return method;
                } else |_| {}

                return self.fail(error.UndefinedField, "Undefined field '{s}'", .{field_access.name.lexeme});
            },
            .enum_type => |u| {
                // Search all variants' fields for the accessed field name
                for (u.variants) |variant| {
                    for (variant.fields) |field| {
                        if (std.mem.eql(u8, field.name, field_access.name.lexeme)) {
                            return .{ .variable = .{ .pipe_type = field.pipe_type, .mutability = field.mutability } };
                        }
                    }
                }

                return self.fail(error.UndefinedType, "Undefined variant '{s}'", .{field_access.name.lexeme});
            },
        };
    }

    fn checkTry(self: *TypeChecker, try_expr: *ast.Expression.Try) !SymbolInfo {
        const inner = try asPipeType(try self.checkExpression(try_expr.expression));

        // Check that the expression is a result enum (synthesized E!T type)
        if (!self.isFallible(inner)) {
            return self.fail(error.TypeMismatch, "Type mismatch: 'try' applied to non-fallible expression at line {d}", .{try_expr.token.line});
        }

        if (!self.in_fallible_fn) {
            return self.fail(error.TypeMismatch, "Type mismatch: 'try' used outside a fallible function at line {d}", .{try_expr.token.line});
        }

        if (try_expr.expression == .variable) {
            _ = self.pending_fallibles.remove(try_expr.expression.variable.token.lexeme);
        }

        // Unwrap the OK variant or propagate the error upwards
        const ok_type = self.getResultVariantType(inner, types.RESULT_OK_VARIANT) orelse return error.TypeMismatch;

        return .{ .variable = .{ .pipe_type = ok_type, .mutability = .constant } };
    }

    fn checkCatch(self: *TypeChecker, catch_expr: *ast.Expression.Catch) !SymbolInfo {
        const left = try asPipeType(try self.checkExpression(catch_expr.expression));
        //
        // Check that the expression is a result enum (synthesized E!T type)
        if (!self.isFallible(left)) {
            return self.fail(error.TypeMismatch, "Type mismatch: 'catch' applied to non-fallible expression at line {d}", .{catch_expr.token.line});
        }

        // Find the ok type from the synthesized res type
        const ok_type = self.getResultVariantType(left, types.RESULT_OK_VARIANT) orelse return error.TypeMismatch;

        // Used in the if, but Zig's defer fires in the block scope, not function scope
        // We need to restore the environment when checkCatch() returns
        const previous = self.env;
        defer self.env = previous;

        // If there's a binding |e|, introduce it in a new scope
        if (catch_expr.binding) |binding| {
            var inner_env = try self.allocator.create(TypeEnvironment);
            inner_env.* = TypeEnvironment.init(self.env, self.allocator);

            // We bind the types.RESULT_ERR_VARIANT value to the inline scope
            const err_type = self.getResultVariantType(left, types.RESULT_ERR_VARIANT) orelse return error.TypeMismatch;
            try inner_env.define(binding.lexeme, .{ .variable = .{ .pipe_type = err_type, .mutability = .constant } });

            self.env = inner_env;
        }

        // Remove the fallible expression *before* checking the handler.
        // It shouldn't see result as pending, catch is the consumption point
        if (catch_expr.expression == .variable) {
            _ = self.pending_fallibles.remove(catch_expr.expression.variable.token.lexeme);
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
    fn asPipeType(info: SymbolInfo) !PipeType {
        return switch (info) {
            .variable => |v| v.pipe_type,
            .function => error.TypeMismatch,
        };
    }

    // Look up a type name (e.g. "Int") and return the corresponding PipeType.
    fn resolveTypeName(self: *TypeChecker, name: []const u8) !PipeType {
        if (type_names.get(name)) |pipe_type| {
            return pipe_type;
        }

        // When the type is "Self", use the current struct type name
        if (std.mem.eql(u8, name, types.SELF_TYPE) and self.current_struct_type != null) {
            return .{ .struct_type = self.current_struct_type.? };
        }

        if (self.type_registry.get(name)) |info| {
            return switch (info) {
                .struct_type => .{ .struct_type = name },
                .enum_type => .{ .enum_type = name },
            };
        }

        return error.UndefinedType;
    }

    const type_names = std.StaticStringMap(PipeType).initComptime(.{
        .{ "Bool", PipeType.bool },
        .{ "Float", PipeType.float },
        .{ "Int", PipeType.int },
        .{ "String", PipeType.string },
        .{ "Unit", PipeType.unit },
    });

    fn resolveTypeAnnotation(self: *TypeChecker, ann: ast.PipeTypeAnnotation) !PipeType {
        return switch (ann) {
            .named => |tok| try self.resolveTypeName(tok.lexeme),
            .inferred_error_union => |inner| {
                // Find the ok type
                const ok_type = try self.allocator.create(PipeType);
                ok_type.* = try self.resolveTypeAnnotation(inner.*);
                return error.NotImplemented;
            },
            .explicit_error_union => |eu| {
                // NOTE: this branch has a side-effect — it synthesizes a new EnumTypeInfo
                // for the E!T result type and registers it in type_registry under the
                // generated name (e.g. "MyError!Int") so later lookups resolve correctly.

                // Guard clauses
                const error_info = self.type_registry.get(eu.error_set.lexeme) orelse return error.UndefinedType;
                if (error_info != .enum_type) {
                    return error.TypeMismatch;
                }

                if (!error_info.enum_type.is_error) {
                    return self.fail(error.TypeMismatch, "Type mismatch: '{s}' is not an error enum. Only error enums are allowed in error position", .{eu.error_set.lexeme});
                }

                // Here we have to build a synthezised type for E!T

                // 1. Find the Ok type
                const ok_type = try self.allocator.create(PipeType);
                ok_type.* = try self.resolveTypeAnnotation(eu.ok_type.*);

                // 2. Generate the E!T name
                const name = try utils.resultTypeName(
                    self.allocator,
                    eu.error_set.lexeme,
                    pipeTypeName(ok_type.*),
                );

                // 3. Synthesize the type
                //
                // EnumTypeInfo (is_result = true)
                // ├── VariantTypeInfo types.RESULT_OK_VARIANT
                // │   └── FieldInfo "value" : ok_type
                // └── VariantTypeInfo types.RESULT_ERR_VARIANT
                //     └── FieldInfo "err" : enum_type(error_set_name)

                // First, the field slices
                const ok_fields = try self.allocator.alloc(types.FieldInfo, 1);
                ok_fields[0] = .{ .name = "value", .pipe_type = ok_type.*, .mutability = .constant };

                const err_fields = try self.allocator.alloc(types.FieldInfo, 1);
                err_fields[0] = .{ .name = "err", .pipe_type = .{ .enum_type = eu.error_set.lexeme }, .mutability = .constant };

                // Then allocate the variant slice
                const variants = try self.allocator.alloc(types.VariantTypeInfo, 2);
                variants[0] = .{ .name = types.RESULT_OK_VARIANT, .fields = ok_fields };
                variants[1] = .{ .name = types.RESULT_ERR_VARIANT, .fields = err_fields };

                // Register and return
                try self.type_registry.put(name, .{ .enum_type = .{
                    .is_error = false,
                    .is_result = true,
                    .variants = variants,
                } });

                return .{ .enum_type = name };
            },
        };
    }

    fn resolveFields(self: *TypeChecker, fields: []const ast.Statement.FieldDeclaration) ![]types.FieldInfo {
        const resolved_fields = try self.allocator.alloc(types.FieldInfo, fields.len);
        for (fields, 0..) |field, i| {
            resolved_fields[i] = .{
                .name = field.name.lexeme,
                .pipe_type = try self.resolveTypeAnnotation(field.type_annotation),
                .mutability = field.mutability,
            };
        }

        return resolved_fields;
    }

    fn registerConstructor(self: *TypeChecker, name: []const u8, fields: []const types.FieldInfo, return_type: PipeType) !void {
        const param_types = try self.allocator.alloc(PipeType, fields.len);
        for (fields, 0..) |fi, i| {
            param_types[i] = fi.pipe_type;
        }

        try self.env.define(name, .{ .function = .{
            .param_types = param_types,
            .return_type = return_type,
        } });
    }

    // Returns true if `from` can be implicitly coerced into `to`.
    // This happens when `to` is a composed enum that has `from` as a nested variant,
    // e.g. `enum AnyRole { StaffRole, Guest }` makes StaffRole coercible to AnyRole.
    fn isNestedEnumCoercible(self: *TypeChecker, from: PipeType, to: PipeType) bool {
        // Both must be enum types — primitives and structs don't participate in this coercion.
        if (from != .enum_type or to != .enum_type) {
            return false;
        }

        // Look up the target enum in the registry to get its variant list.
        const to_name = to.enum_type;
        const info = self.type_registry.get(to_name) orelse return false;
        const to_info = switch (info) {
            .enum_type => |u| u,
            else => return false,
        };

        // A nested union variant is stored as a single synthetic field whose pipe_type
        // is the inner union type. Check if any variant field matches `from`.
        for (to_info.variants) |variant| {
            for (variant.fields) |field| {
                // TODO: Recurse into a nested union variants once when with exhaustive
                // matching is implemented
                if (field.pipe_type.compatible(from)) {
                    return true;
                }
            }
        }

        return false;
    }

    // Like PipeType.compatible but also allows T where E!T is expected (implicit Ok wrapping).
    fn isCompatible(self: *TypeChecker, from: PipeType, to: PipeType) bool {
        if (from.compatible(to)) return true;
        if (self.getResultVariantType(to, types.RESULT_OK_VARIANT)) |ok_type| {
            return from.compatible(ok_type);
        }
        return false;
    }

    // Return true if the type is a result enum
    fn isFallible(self: *TypeChecker, t: PipeType) bool {
        if (t != .enum_type) {
            return false;
        }

        const info = self.type_registry.get(t.enum_type) orelse return false;
        return info.enum_type.is_result;
    }

    // Returns the field type of a named variant in a result enum, or null
    fn getResultVariantType(self: *TypeChecker, t: PipeType, variant_name: []const u8) ?PipeType {
        if (t != .enum_type) {
            return null;
        }
        const info = self.type_registry.get(t.enum_type) orelse return null;

        if (!info.enum_type.is_result) {
            return null;
        }

        // Iterate through the variants to find the variant with the given name
        for (info.enum_type.variants) |v| {
            if (std.mem.eql(u8, v.name, variant_name)) {
                if (v.fields.len == 1) {
                    return v.fields[0].pipe_type;
                }
            }
        }

        return null;
    }

    fn expectUnwrapped(self: *TypeChecker, t: PipeType, line: usize) !PipeType {
        if (self.isFallible(t)) {
            return self.fail(error.TypeMismatch, "!T value must be unwrapped with try, catch, or when at line {d}", .{line});
        }
        return t;
    }

    // Returns the name of a PipeType
    fn pipeTypeName(t: PipeType) []const u8 {
        return switch (t) {
            .int => "Int",
            .string => "String",
            .bool => "Bool",
            .float => "Float",
            .unit => "Unit",
            .any => "Any",
            .struct_type => |name| name,
            .enum_type => |name| name,
        };
    }

    // Returns true if the two sets of pending errors are equal
    fn pendingSetsEqual(a: std.StringHashMap(void), b: std.StringHashMap(void)) bool {
        if (a.count() != b.count()) {
            return false;
        }

        var it = a.keyIterator();
        while (it.next()) |name| {
            if (!b.contains(name.*)) {
                return false;
            }
        }

        return true;
    }
};
