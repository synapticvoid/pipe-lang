const std = @import("std");
const ast = @import("../ast.zig");
const builtins = @import("builtins.zig");
const token = @import("../token.zig");
const types = @import("../types.zig");
const utils = @import("../utils.zig");

const RuntimeContext = @import("../runtime.zig").RuntimeContext;
const Environment = @import("environment.zig").Environment;
const Callable = @import("callable.zig").Callable;
const Value = @import("value.zig").Value;

const Expression = ast.Expression;
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
    // Set of declared enum type names; used to detect when a zero-field variant references
    // an existing enum (nested enum coercion at runtime)
    known_enum_names: std.StringHashMapUnmanaged(void),
    ctx: RuntimeContext,
    return_value: ?Value = null,
    method_receiver: ?Value = null,
    allocator: std.mem.Allocator,

    pub fn init(ctx: RuntimeContext, allocator: std.mem.Allocator) !Interpreter {
        const env = try allocator.create(Environment);
        env.* = Environment.init(null, allocator);
        try builtins.registerAll(env);

        return .{
            .env = env,
            .known_enum_names = .{},
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
            .struct_declaration => |decl| try self.executeStructDeclarationStatement(decl),
            .enum_declaration => |decl| try self.executeEnumDeclaration(decl),
        }
    }

    fn executeVarDeclarationStatement(self: *Interpreter, decl: ast.Statement.VarDeclaration) !void {
        const value = if (decl.initializer) |init_expr| try self.evaluate(init_expr) else Value.null;
        try self.env.define(decl.name.lexeme, value);
    }

    fn executeFnDeclarationStatement(self: *Interpreter, decl: ast.Statement.FnDeclaration) !void {
        // For fallible functions (return type E!T), compute the synthesized result enum
        // name (e.g. MathError!Int) so that callUserFn can wrap the return value
        // in the correct Ok/Err variant at runtime.
        // For non fallible functions (no return type, or plain named return), this is null
        const result_name: ?[]const u8 = if (decl.return_type) |ret| switch (ret) {
            .explicit_error_union => |eu| blk: {
                // ok_type is a *PipeTypeAnnotation - switch on the pointee to extract the name.
                // Complex ok types (nested error unions) are not supported yet
                const ok_name: ?[]const u8 = switch (eu.ok_type.*) {
                    .named => |tok| tok.lexeme,
                    else => null,
                };

                if (ok_name) |name| {
                    break :blk try utils.resultTypeName(self.allocator, eu.error_set.lexeme, name);
                }
                break :blk null;
            },
            else => null,
        } else null;

        const user_fn = Callable.UserFn{ .closure = self.env, .declaration = decl, .result_name = result_name };
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

        const body_field_names = try self.allocator.alloc([]const u8, decl.body_fields.len);
        const body_field_defaults = try self.allocator.alloc(ast.Expression, decl.body_fields.len);
        for (decl.body_fields, 0..) |field, i| {
            body_field_names[i] = field.name.lexeme;
            body_field_defaults[i] = field.default_value orelse unreachable;
        }

        // Define the struct constructor as a function "Type"
        try self.env.define(decl.name.lexeme, .{ .function = .{ .struct_constructor = .{
            .name = decl.name.lexeme,
            .field_names = field_names,
            .body_field_names = body_field_names,
            .body_field_defaults = body_field_defaults,
            .kind = decl.kind,
        } } });

        // Register the methods as "Type.method"
        for (decl.methods) |method| {
            const qualified = try utils.memberName(self.allocator, decl.name.lexeme, method.name.lexeme);
            const user_fn = Callable.UserFn{
                .closure = self.env,
                .declaration = method,
                .result_name = null,
            };
            try self.env.define(qualified, .{ .function = .{ .user = user_fn } });
        }
    }

    fn executeEnumDeclaration(self: *Interpreter, decl: ast.Statement.EnumDeclaration) !void {
        for (decl.variants) |variant| {
            const field_names: [][]const u8 = blk: {
                // Branch when we reference an existing enum
                if (variant.fields.len != 0) break :blk null;
                if (!self.known_enum_names.contains(variant.name.lexeme)) break :blk null;

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

            const constructor_name = try utils.memberName(self.allocator, decl.name.lexeme, variant.name.lexeme);
            try self.env.define(constructor_name, .{ .function = .{ .struct_constructor = .{
                .name = constructor_name,
                .field_names = field_names,
                .body_field_names = &.{},
                .body_field_defaults = &.{},
                .kind = .case,
            } } });
        }

        // Don't forget to add the enum to our known enum names
        try self.known_enum_names.put(self.allocator, decl.name.lexeme, {});
    }

    // NOTE: -- Expressions

    pub fn evaluate(self: *Interpreter, expr: ast.Expression) InterpreterError!Value {
        return switch (expr) {
            .literal => |e| switch (e.value) {
                .int => |n| .{ .int = n },
                .string => |s| .{ .string = s },
                .boolean => |b| .{ .boolean = b },
                .null => .null,
                .unit => .unit,
            },
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
            .field_assignment => |e| self.evaluateFieldAssignment(e),
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
            .equal_equal => return Value{ .boolean = try self.evaluateEquality(left, right) },
            .bang_equal => return Value{ .boolean = !try self.evaluateEquality(left, right) },
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
                    // Evalute arguments
                    for (e.args) |arg| {
                        try args.append(self.allocator, try self.evaluate(arg));
                    }

                    for (args.items, 0..) |arg, i| {
                        if (arg == .struct_instance) {
                            const qualified = try utils.memberName(self.allocator, arg.struct_instance.type_name, types.METHOD_TO_STRING);
                            if (self.env.get(qualified)) |method_val| {
                                const user_fn = method_val.function.user;
                                const env = try self.allocator.create(Environment);
                                env.* = Environment.init(user_fn.closure, self.allocator);

                                try env.define(user_fn.declaration.params[0].name.lexeme, arg);

                                args.items[i] = try self.evaluateBlock(user_fn.declaration.body, env);
                            } else |_| {}
                        }
                    }

                    return builtin_fn.func(args.items, self.ctx);
                },
                .struct_constructor => |ctor| {
                    // Evalute constructor fields
                    const values = try self.allocator.alloc(Value, e.args.len);
                    for (e.args, 0..) |arg, i| {
                        values[i] = try self.evaluate(arg);
                    }

                    // Evaluate body fields
                    const body_values = try self.allocator.alloc(Value, ctor.body_field_names.len);
                    for (ctor.body_field_defaults, 0..) |default, i| {
                        body_values[i] = try self.evaluate(default);
                    }

                    const instance = try self.allocator.create(Value.StructInstance);
                    instance.* = .{
                        .type_name = ctor.name,
                        .field_names = ctor.field_names,
                        .field_values = values,
                        .body_field_names = ctor.body_field_names,
                        .body_field_values = body_values,
                        .kind = ctor.kind,
                    };
                    return Value{ .struct_instance = instance };
                },
            },
            else => return InterpreterError.TypeError,
        }
    }

    fn evaluateTry(self: *Interpreter, e: *Expression.Try) InterpreterError!Value {
        const value = try self.evaluate(e.expression);
        const si = switch (value) {
            .struct_instance => |si| si,
            else => return error.TypeError,
        };

        const type_name = si.type_name;

        // It's the OK variant, we can unwrap the success value
        // ++ is a compile-time string concat
        if (std.mem.endsWith(u8, type_name, "." ++ types.RESULT_OK_VARIANT)) {
            return si.field_values[0];
        }

        // It's the Err variant, store the full error variant and raise ErrorSignal
        if (std.mem.endsWith(u8, type_name, "." ++ types.RESULT_ERR_VARIANT)) {
            self.return_value = value;
            return error.ErrorSignal;
        }

        return error.TypeError;
    }

    fn evaluateCatch(self: *Interpreter, e: *Expression.Catch) InterpreterError!Value {
        // Execute left side and check if it's a struct_instance
        const left = try self.evaluate(e.expression);

        const si = switch (left) {
            .struct_instance => |si| si,
            else => return error.TypeError,
        };

        const type_name = si.type_name;

        // It's the OK variant, we can unwrap the success value
        // ++ is a compile-time string concat
        if (std.mem.endsWith(u8, type_name, "." ++ types.RESULT_OK_VARIANT)) {
            return si.field_values[0];
        }

        // It's the Err variant, execute the handle with a new environment if needed
        if (std.mem.endsWith(u8, type_name, "." ++ types.RESULT_ERR_VARIANT)) {
            const previous = self.env;
            defer self.env = previous;

            if (e.binding) |binding| {
                const env = try self.allocator.create(Environment);
                env.* = Environment.init(self.env, self.allocator);
                // Bind the raw value of the Err variant
                try env.define(binding.lexeme, si.field_values[0]);
                self.env = env;
            }

            return try self.evaluate(e.handler);
        }

        return error.TypeError;
    }

    fn evaluateStructInit(_: *Interpreter, _: *Expression.StructInit) InterpreterError!Value {
        // Struct construction is handled via evaluateFnCall + struct_constructor callable.
        // This path is unreachable because the parser never produces struct_init nodes.
        unreachable;
    }

    fn evaluateFieldAccess(self: *Interpreter, e: *Expression.FieldAccess) InterpreterError!Value {
        // Check for qualified union variant constructor: Role.Member
        if (e.object == .variable) {
            const qualified = try utils.memberName(self.allocator, e.object.variable.token.lexeme, e.name.lexeme);
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

        // Find the body field and return its value
        for (inst.body_field_names, inst.body_field_values) |name, value| {
            if (std.mem.eql(u8, name, e.name.lexeme)) {
                return value;
            }
        }

        // Check for instance method
        const qualified = try utils.memberName(self.allocator, inst.type_name, e.name.lexeme);
        if (self.env.get(qualified)) |method| {
            // Store the instance so evaluateFnCall can injedct it as self
            self.method_receiver = obj;
            return method;
        } else |_| {}

        return InterpreterError.UndefinedField;
    }

    fn evaluateFieldAssignment(self: *Interpreter, e: *Expression.FieldAssignment) InterpreterError!Value {
        // Evaluate object first
        const obj = try self.evaluate(e.object);

        const inst = switch (obj) {
            .struct_instance => |ptr| ptr,
            else => return InterpreterError.TypeError,
        };

        // Find field index in field_names and update it's value
        for (inst.field_names, 0..) |name, i| {
            if (std.mem.eql(u8, name, e.name.lexeme)) {
                const value = try self.evaluate(e.value);
                inst.field_values[i] = value;
                return value;
            }
        }

        return error.UndefinedField;
    }

    fn evaluateBlock(self: *Interpreter, statements: []const ast.Statement, env: *Environment) !Value {
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

        // Methods have self as first param, skip it to properly match args
        const params = if (self.method_receiver != null)
            function.declaration.params[1..]
        else
            function.declaration.params;

        // Bind method receiver as first param if present
        if (self.method_receiver) |receiver| {
            try env.define(function.declaration.params[0].name.lexeme, receiver);
            self.method_receiver = null;
        }

        for (args, params) |arg, param| {
            const value = try self.evaluate(arg);
            try env.define(param.name.lexeme, value);
        }

        // Execute function's body
        const raw = self.evaluateBlock(function.declaration.body, env) catch |err| switch (err) {
            error.ReturnSignal => blk: {
                const v = self.return_value orelse Value.unit;
                self.return_value = null;
                break :blk v;
            },
            error.ErrorSignal => {
                const err_val = self.return_value orelse Value.unit;
                self.return_value = null;
                return err_val; // Pass through Err instance, skip Ok wrapping
            },
            else => return err,
        };

        // If the function is a result (E!T), we need to either
        // wrap in Err or Ok
        if (function.result_name) |name| {
            // If we have name = "MathError!Int", extract "MathError"
            const bang_idx = std.mem.indexOf(u8, name, "!") orelse name.len;
            const err_prefix = name[0..bang_idx];

            // raw return value of the fn is a struct that starts with our E prefix
            if (raw == .struct_instance and std.mem.startsWith(u8, raw.struct_instance.type_name, err_prefix)) {
                const type_name = try utils.memberName(self.allocator, name, types.RESULT_ERR_VARIANT);
                const values = try self.allocator.alloc(Value, 1);
                values[0] = raw;

                const field_names = try self.allocator.alloc([]const u8, 1);
                field_names[0] = "err";

                const instance = try self.allocator.create(Value.StructInstance);
                instance.* = .{
                    .type_name = type_name,
                    .field_names = field_names,
                    .field_values = values,
                    .body_field_names = &.{},
                    .body_field_values = &.{},
                    .kind = .case,
                };
                return Value{ .struct_instance = instance };
            }

            // If an Ok, wrap it and return the instance
            const type_name = try utils.memberName(self.allocator, name, types.RESULT_OK_VARIANT);
            const values = try self.allocator.alloc(Value, 1);
            values[0] = raw;

            const field_names = try self.allocator.alloc([]const u8, 1);
            field_names[0] = "value";

            const instance = try self.allocator.create(Value.StructInstance);
            instance.* = .{
                .type_name = type_name,
                .field_names = field_names,
                .field_values = values,
                .body_field_names = &.{},
                .body_field_values = &.{},
                .kind = .case,
            };

            return Value{ .struct_instance = instance };
        }

        return raw;
    }

    // NOTE: -- Helpers

    fn evaluateEquality(self: *Interpreter, left: Value, right: Value) InterpreterError!bool {
        if (left == .struct_instance and right == .struct_instance) {
            const type_name = left.struct_instance.type_name;
            const qualified = try utils.memberName(self.allocator, type_name, types.METHOD_EQUALS);
            if (self.env.get(qualified)) |method_val| {
                const user_fn = method_val.function.user;

                const env = try self.allocator.create(Environment);
                env.* = Environment.init(user_fn.closure, self.allocator);

                try env.define(user_fn.declaration.params[0].name.lexeme, left);
                try env.define(user_fn.declaration.params[1].name.lexeme, right);

                return (try self.evaluateBlock(user_fn.declaration.body, env)).isTruthy();
            } else |_| {}
        }
        return left.eql(right);
    }
};
