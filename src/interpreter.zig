const std = @import("std");
const ast = @import("ast.zig");
const builtins = @import("builtins.zig");
const token = @import("token.zig");
const utils = @import("utils.zig");

const RuntimeContext = @import("runtime.zig").RuntimeContext;
const Environment = @import("environment.zig").Environment;
const Callable = @import("callable.zig").Callable;
const Expression = ast.Expression;
const Value = ast.Value;
const Token = token.Token;
const TokenType = token.TokenType;

pub const InterpreterError = error{
    NotImplemented,
    OutOfMemory,
    TypeError,
    UndefinedField,
    UndefinedVariable,
    UndefinedType,
    UnsupportedOperator,
    VariableAlreadyDefined,

    // Control flow
    ReturnSignal,
    ErrorSignal,
};

pub const Interpreter = struct {
    env: *Environment,
    // Lookup registry to match union name (used for type coercion)
    known_union_names: std.StringHashMap(void),
    ctx: RuntimeContext,
    return_value: ?Value = null,
    error_value: ?Value = null,
    allocator: std.mem.Allocator,

    pub fn init(ctx: RuntimeContext, allocator: std.mem.Allocator) !Interpreter {
        const env = try allocator.create(Environment);
        env.* = Environment.init(null, allocator);
        try builtins.registerAll(env);

        return .{
            .env = env,
            .known_union_names = std.StringHashMap(void).init(allocator),
            .ctx = ctx,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Interpreter) void {
        self.env.deinit();
    }

    pub fn interpret(self: *Interpreter, statements: []const ast.Statement) !void {
        for (statements) |statement| {
            try self.execute(statement);
        }
    }

    // NOTE: -- Statements

    pub fn execute(self: *Interpreter, statement: ast.Statement) !void {
        switch (statement) {
            .var_declaration => |decl| try self.executeVarDeclarationStatement(decl),
            .fn_declaration => |decl| try self.executeFnDeclarationStatement(decl),
            .expression => |expr| _ = try self.evaluate(expr),
            .@"return" => |r| try self.executeReturnStatement(r),
            .error_declaration => |decl| _ = decl,
            .error_union_declaration => |decl| _ = decl,
            .struct_declaration => |decl| try self.executeStructDeclarationStatement(decl),
            .union_declaration => |decl| try self.executeUnionDeclaration(decl),
        }
    }

    fn executeVarDeclarationStatement(self: *Interpreter, decl: ast.Statement.VarDeclaration) !void {
        const value = if (decl.initializer) |init_expr| try self.evaluate(init_expr) else Value.null;
        try self.env.define(decl.name.lexeme, value);
    }

    fn executeFnDeclarationStatement(self: *Interpreter, decl: ast.Statement.FnDeclaration) !void {
        const user_fn = Callable.UserFn{ .closure = self.env, .declaration = decl };
        try self.env.define(decl.name.lexeme, .{ .function = .{ .user = user_fn } });
    }

    fn executeReturnStatement(self: *Interpreter, ret: ast.Statement.Return) !void {
        if (ret.value) |value| {
            self.return_value = try self.evaluate(value);
        } else {
            self.return_value = .unit;
        }
        return error.ReturnSignal;
    }

    fn executeStructDeclarationStatement(self: *Interpreter, decl: ast.Statement.StructDeclaration) !void {
        const field_names = try self.allocator.alloc([]const u8, decl.fields.len);
        for (decl.fields, 0..) |field, i| {
            field_names[i] = field.name.lexeme;
        }

        try self.env.define(decl.name.lexeme, .{ .function = .{ .struct_constructor = .{
            .name = decl.name.lexeme,
            .field_names = field_names,
            .kind = decl.kind,
        } } });
    }

    fn executeUnionDeclaration(self: *Interpreter, decl: ast.Statement.UnionDeclaration) !void {
        for (decl.variants) |variant| {
            const field_names: [][]const u8 = blk: {
                // Branch when we reference an existing union
                if (variant.fields.len != 0) break :blk null;
                if (!self.known_union_names.contains(variant.name.lexeme)) break :blk null;

                // Create a single-field variant for the constructor
                const names = try self.allocator.alloc([]const u8, 1);
                names[0] = variant.name.lexeme;
                break :blk names;
            } orelse blk: {
                const names = try self.allocator.alloc([]const u8, variant.fields.len);
                for (variant.fields, 0..) |field, i| {
                    names[i] = field.name.lexeme;
                }
                break :blk names;
            };

            const constructor_name = try utils.qualifiedName(self.allocator, decl.name.lexeme, variant.name.lexeme);
            try self.env.define(constructor_name, .{ .function = .{ .struct_constructor = .{
                .name = constructor_name,
                .field_names = field_names,
                .kind = .case,
            } } });
        }

        // Don't forget to add the union to our known union names
        try self.known_union_names.put(decl.name.lexeme, {});
    }

    // NOTE: -- Expressions

    pub fn evaluate(self: *Interpreter, expr: ast.Expression) InterpreterError!Value {
        return switch (expr) {
            .literal => |e| e.value,
            .variable => |e| self.env.get(e.token.lexeme),
            .var_assignment => |e| self.evaluateVarAssignment(e),
            .unary => |e| self.evaluateUnary(e),
            .binary => |e| self.evaluateBinary(e),
            .block => |e| {
                const env = try self.allocator.create(Environment);
                env.* = Environment.init(self.env, self.allocator);
                return try self.evaluateBlock(e.statements, env);
            },
            .if_expr => |e| self.evaluateIf(e),
            .fn_call => |e| self.evaluateFnCall(e),
            .try_expr => |e| self.evaluateTry(e),
            .catch_expr => |e| self.evaluateCatch(e),
            .struct_init => |e| self.evaluateStructInit(e),
            .field_access => |e| self.evaluateFieldAccess(e),
        };
    }

    fn evaluateVarAssignment(self: *Interpreter, e: *Expression.VarAssignment) InterpreterError!Value {
        const value = try self.evaluate(e.value);
        try self.env.assign(e.token.lexeme, value);
        return value;
    }

    fn evaluateUnary(self: *Interpreter, e: *Expression.Unary) InterpreterError!Value {
        const right = try self.evaluate(e.right);
        switch (e.operator.type) {
            .bang => return Value{ .boolean = !right.isTruthy() },
            .minus => return Value{ .int = -(try right.asInt()) },
            else => return InterpreterError.UnsupportedOperator,
        }
    }

    fn evaluateBinary(self: *Interpreter, e: *Expression.Binary) InterpreterError!Value {
        const left = try self.evaluate(e.left);
        const right = try self.evaluate(e.right);

        // Handle equality before (comparing struct, etc)
        switch (e.operator.type) {
            .equal_equal => return Value{ .boolean = left.eql(right) },
            .bang_equal => return Value{ .boolean = !left.eql(right) },
            else => {},
        }

        const left_val = try left.asInt();
        const right_val = try right.asInt();

        switch (e.operator.type) {
            .plus => return Value{ .int = left_val + right_val },
            .minus => return Value{ .int = left_val - right_val },
            .star => return Value{ .int = left_val * right_val },
            .slash => return Value{ .int = @divTrunc(left_val, right_val) },
            .greater => return Value{ .boolean = left_val > right_val },
            .greater_equal => return Value{ .boolean = left_val >= right_val },
            .less => return Value{ .boolean = left_val < right_val },
            .less_equal => return Value{ .boolean = left_val <= right_val },

            else => return InterpreterError.UnsupportedOperator,
        }
    }

    fn evaluateIf(self: *Interpreter, e: *Expression.If) InterpreterError!Value {
        const cond = try self.evaluate(e.condition);
        if (cond.isTruthy()) {
            return try self.evaluate(e.then_branch);
        } else if (e.else_branch) |else_expr| {
            return try self.evaluate(else_expr);
        }
        return Value.unit;
    }

    fn evaluateFnCall(self: *Interpreter, e: *Expression.FnCall) InterpreterError!Value {
        const function = try self.evaluate(e.callee);
        switch (function) {
            .function => |callable| switch (callable) {
                .user => |user_fn| return try self.callUserFn(user_fn, e.args),
                .builtin => |builtin_fn| {
                    var args: std.ArrayList(Value) = .{};
                    for (e.args) |arg| {
                        try args.append(self.allocator, try self.evaluate(arg));
                    }
                    return builtin_fn.func(args.items, self.ctx);
                },
                .struct_constructor => |ctor| {
                    const values = try self.allocator.alloc(Value, e.args.len);
                    for (e.args, 0..) |arg, i| {
                        values[i] = try self.evaluate(arg);
                    }
                    const instance = try self.allocator.create(Value.StructInstance);
                    instance.* = .{
                        .type_name = ctor.name,
                        .field_names = ctor.field_names,
                        .field_values = values,
                        .kind = ctor.kind,
                    };
                    return Value{ .struct_instance = instance };
                },
            },
            else => return InterpreterError.TypeError,
        }
    }

    fn evaluateTry(self: *Interpreter, e: *Expression.Try) InterpreterError!Value {
        const result = try self.evaluate(e.expression);
        return switch (result) {
            .error_value => {
                self.error_value = result;
                return error.ErrorSignal;
            },
            else => result,
        };
    }

    fn evaluateCatch(self: *Interpreter, e: *Expression.Catch) InterpreterError!Value {
        const left = try self.evaluate(e.expression);
        return switch (left) {
            .error_value => {
                const previous = self.env;
                defer self.env = previous;

                if (e.binding) |binding| {
                    const env = try self.allocator.create(Environment);
                    env.* = Environment.init(self.env, self.allocator);
                    try env.define(binding.lexeme, left);
                    self.env = env;
                }

                return try self.evaluate(e.handler);
            },
            else => left,
        };
    }

    fn evaluateStructInit(_: *Interpreter, _: *Expression.StructInit) InterpreterError!Value {
        // Struct construction is handled via evaluateFnCall + struct_constructor callable.
        // This path is unreachable because the parser never produces struct_init nodes.
        unreachable;
    }

    fn evaluateFieldAccess(self: *Interpreter, e: *Expression.FieldAccess) InterpreterError!Value {
        // Check for qualified union variant constructor: Role.Member
        if (e.object == .variable) {
            const qualified = try utils.qualifiedName(self.allocator, e.object.variable.token.lexeme, e.name.lexeme);
            if (self.env.get(qualified)) |val| {
                return val;
            } else |_| {}
        }

        // Evaluate object first
        const obj = try self.evaluate(e.object);
        const inst = switch (obj) {
            .struct_instance => |ptr| ptr.*,
            else => return InterpreterError.TypeError,
        };

        // Find the field and return its value
        for (inst.field_names, inst.field_values) |name, value| {
            if (std.mem.eql(u8, name, e.name.lexeme)) {
                return value;
            }
        }

        return InterpreterError.UndefinedField;
    }

    fn evaluateBlock(self: *Interpreter, statements: []const ast.Statement, env: *Environment) !ast.Value {
        const previous = self.env;
        self.env = env;
        defer self.env = previous;

        if (statements.len == 0) {
            return Value.unit;
        }

        // Execute all statements except the last one
        for (statements[0 .. statements.len - 1]) |statement| {
            try self.execute(statement);
        }

        // We have to extract the value of the last statement
        const last = statements[statements.len - 1];
        return switch (last) {
            .expression => |expr| try self.evaluate(expr),
            else => {
                try self.execute(last);
                return Value.unit;
            },
        };
    }

    pub fn callUserFn(self: *Interpreter, function: Callable.UserFn, args: []const Expression) !Value {
        // Create environment with params
        const env = try self.allocator.create(Environment);
        env.* = Environment.init(function.closure, self.allocator);
        for (args, function.declaration.params) |arg, param| {
            const value = try self.evaluate(arg);
            try env.define(param.name.lexeme, value);
        }

        // Execute function's body
        return self.evaluateBlock(function.declaration.body, env) catch |err| switch (err) {
            error.ReturnSignal => {
                const value = self.return_value orelse Value.unit;
                self.return_value = null;
                return value;
            },
            error.ErrorSignal => {
                const value = self.error_value orelse Value.unit;
                self.error_value = null;
                return value;
            },
            else => return err,
        };
    }
};
