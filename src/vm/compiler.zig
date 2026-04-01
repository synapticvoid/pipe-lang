const std = @import("std");

const utils = @import("../utils.zig");
const types = @import("../types.zig");

const Chunk = @import("chunk.zig").Chunk;
const ChunkError = @import("chunk.zig").ChunkError;
const OpCode = @import("opcode.zig").OpCode;

const prg = @import("program.zig");
const Program = prg.Program;
const FnObject = prg.FnObject;
const StructDef = prg.StructDef;

const ast = @import("../ast.zig");
const Value = @import("value.zig").Value;

const Local = struct {
    name: []const u8,
    // nesting depth at which this local was declared
    depth: u32,
};

pub const CompileError = error{
    UnsupportedNode,
    UndefinedStruct,
    UndefinedVariable,
    TooManyLocals,
    OutOfMemory,
} || ChunkError;

pub const Compiler = struct {
    program: *Program,
    chunk: *Chunk,
    locals: std.ArrayList(Local),
    scope_depth: u32,
    // Set of declared enum type names; used to detect when a zero-field variant references
    // an existing enum (nested enum coercion at runtime)
    known_enum_names: std.StringHashMapUnmanaged(void),
    allocator: std.mem.Allocator,

    // Static entry point to compile statements to a Program
    pub fn compile(statements: []const ast.Statement, allocator: std.mem.Allocator) CompileError!Program {
        var program = Program.init(allocator);
        var compiler = Compiler{
            .program = &program,
            .chunk = &program.chunk,
            .locals = .{},
            .scope_depth = 0,
            .known_enum_names = .{},
            .allocator = allocator,
        };
        errdefer compiler.deinit();

        try compiler.compileStatements(statements);
        try compiler.emitOp(.@"return", 0);
        return program;
    }

    pub fn init(program: *Program, allocator: std.mem.Allocator) Compiler {
        return .{
            .program = program,
            .chunk = &program.chunk,
            .locals = .{},
            .scope_depth = 0,
            .known_enum_names = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit(self.allocator);
        self.known_enum_names.deinit(self.allocator);
    }

    // NOTE: -- Statements

    pub fn compileStatements(self: *Compiler, statements: []const ast.Statement) CompileError!void {
        for (statements) |statement| {
            try self.compileStatement(statement);
        }
    }

    pub fn compileStatement(self: *Compiler, statement: ast.Statement) CompileError!void {
        switch (statement) {
            .expression => |expr| {
                try self.compileExpression(expr);
                try self.emitOp(OpCode.pop, expr.line());
            },
            .var_declaration => |var_decl| try self.compileVarDeclarationStatement(var_decl),
            .fn_declaration => |fn_decl| try self.compileFnDeclarationStatement(fn_decl),
            .struct_declaration => |struct_decl| try self.compileStructDeclarationStatement(struct_decl),
            .enum_declaration => |ed| try self.compileEnumDeclarationStatement(ed),
            .@"return" => |ret| try self.compileReturnStatement(ret),
        }
    }

    fn compileVarDeclarationStatement(self: *Compiler, var_decl: ast.Statement.VarDeclaration) CompileError!void {
        if (var_decl.initializer) |in| {
            try self.compileExpression(in);
        }
        try self.declareLocal(var_decl.name.lexeme);
    }

    fn compileFnDeclarationStatement(self: *Compiler, fn_decl: ast.Statement.FnDeclaration) CompileError!void {
        // For fallible functions (return type E!T), compute the synthesized result enum
        // name (e.g. MathError!Int) so that callUserFn can wrap the return value
        // in the correct Ok/Err variant at runtime.
        // For non fallible functions (no return type, or plain named return), this is null
        const result_name: ?[]const u8 = if (fn_decl.return_type) |ret| switch (ret) {
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

        var fn_obj = FnObject{
            .name = fn_decl.name.lexeme,
            .arity = @intCast(fn_decl.params.len),
            .chunk = Chunk.init(self.allocator),
            .result_name = result_name,
        };

        // Save compiler state
        const prev_chunk = self.chunk;
        const prev_locals = self.locals;
        const prev_scope_depth = self.scope_depth;

        // Switch to the function's chunk
        self.chunk = &fn_obj.chunk;
        self.locals = .{};
        self.scope_depth = 0;

        // Reserve slot 0 for the function itself
        // Useful for closures, methods, etc.
        try self.declareLocal(fn_decl.name.lexeme);

        // Declare function params as locals
        for (fn_decl.params) |param| {
            try self.declareLocal(param.name.lexeme);
        }

        // Now, comile the body!
        _ = try self.compileBody(fn_decl.body);

        // Implicit return in case the function falls through without an explicit return
        try self.emitOp(OpCode.@"return", fn_decl.name.line);

        // Restore compiler state
        self.chunk = prev_chunk;
        self.locals.deinit(self.allocator);
        self.locals = prev_locals;
        self.scope_depth = prev_scope_depth;

        // Emit the fn object into the enclosing chunk
        const fn_idx = try self.program.addFunction(fn_obj);
        try self.emitConstant(.{ .function = fn_idx }, fn_decl.name.line);

        // Declare the function name as a local (functions are first-class values)
        // If we are the the top-level, store as a global
        if (self.scope_depth == 0) {
            const name_idx = try self.chunk.findOrAddConstant(.{ .string = fn_decl.name.lexeme });
            try self.emitOp(OpCode.define_global, fn_decl.name.line);
            try self.emitU16(name_idx, fn_decl.name.line);
        } else {
            try self.declareLocal(fn_decl.name.lexeme);
        }
    }

    fn compileStructDeclarationStatement(self: *Compiler, struct_decl: ast.Statement.StructDeclaration) CompileError!void {
        // Allocate field names
        const field_names = try self.allocator.alloc([]const u8, struct_decl.fields.len);
        for (struct_decl.fields, 0..) |field, i| {
            field_names[i] = field.name.lexeme;
        }

        // Allocate body field names
        const body_field_names = try self.allocator.alloc([]const u8, struct_decl.body_fields.len);
        for (struct_decl.body_fields, 0..) |field, i| {
            body_field_names[i] = field.name.lexeme;
        }

        const struct_obj = StructDef{
            .name = struct_decl.name.lexeme,
            .kind = struct_decl.kind,
            .field_names = field_names,
            .body_field_names = body_field_names,
            .body_default_fn = null,
        };

        _ = try self.program.addStructDef(struct_obj);
    }

    fn compileEnumDeclarationStatement(self: *Compiler, ed: ast.Statement.EnumDeclaration) CompileError!void {
        const enum_name = ed.name.lexeme;
        for (ed.variants) |variant| {
            // Build the qualified name EnumName.VariantName
            const variant_name = variant.name.lexeme;
            const type_name = try self.buildEnumVariantQualifiedName(enum_name, variant_name);

            var field_names: [][]const u8 = undefined;

            // Nesting an enum inside another?
            // e.g: enum StaffRole { Admin, Member };
            // enum AnyRole { StaffRole, Guest };
            // When compiling AnyRole, StaffRole is already defined.
            if (variant.fields.len == 0 and self.known_enum_names.contains(variant_name)) {
                // Rather than redefining the enum, just reference it
                const names = try self.allocator.alloc([]const u8, 1);
                names[0] = variant_name;
                field_names = names;
            } else {
                // Collect field_names
                field_names = try self.allocator.alloc([]const u8, variant.fields.len);
                for (variant.fields, 0..) |f, i| {
                    field_names[i] = f.name.lexeme;
                }
            }

            // The struct def is always a .case kind (for equality)
            _ = try self.program.addStructDef(.{
                .name = type_name,
                .kind = .case,
                .field_names = field_names,
                .body_field_names = &.{},
                .body_default_fn = null,
            });
        }

        // Don't forget to add the enum to our known enum names
        try self.known_enum_names.put(self.allocator, enum_name, {});
    }

    fn compileReturnStatement(self: *Compiler, ret: ast.Statement.Return) CompileError!void {
        if (ret.value) |value| {
            try self.compileExpression(value);
        }
        try self.emitOp(OpCode.@"return", ret.token.line);
    }

    // NOTE: -- Expressions

    pub fn compileExpression(self: *Compiler, expr: ast.Expression) CompileError!void {
        switch (expr) {
            .literal => |lit| try self.compileLiteral(lit),
            .unary => |u| try self.compileUnary(u),
            .binary => |b| try self.compileBinary(b),
            .variable => |v| try self.compileVariable(v),
            .var_assignment => |v| try self.compileVarAssignement(v),
            .block => |b| try self.compileBlock(b),
            .if_expr => |i| try self.compileIf(i),
            .fn_call => |f| try self.compileFnCall(f),
            .struct_init => |si| try self.compileStructInit(si),
            .field_access => |fa| try self.compileFieldAccess(fa),
            .field_assignment => |fa| try self.compileFieldAssignment(fa),
            .try_expr => |t| try self.compileTry(t),
            .catch_expr => |c| try self.compileCatch(c),
        }
    }

    fn compileLiteral(self: *Compiler, lit: ast.Expression.Literal) CompileError!void {
        switch (lit.value) {
            .int => |n| try self.emitConstant(.{ .int = n }, lit.token.line),
            .string => |s| try self.emitConstant(.{ .string = s }, lit.token.line),
            .boolean => |b| switch (b) {
                true => try self.emitOp(OpCode.true, lit.token.line),
                false => try self.emitOp(OpCode.false, lit.token.line),
            },
            .null => try self.emitOp(OpCode.null, lit.token.line),
            else => return error.UnsupportedNode,
        }
    }

    fn compileUnary(self: *Compiler, unary: *const ast.Expression.Unary) CompileError!void {
        try self.compileExpression(unary.right);
        switch (unary.operator.type) {
            .bang => try self.emitOp(OpCode.not, unary.operator.line),
            .minus => try self.emitOp(OpCode.negate, unary.operator.line),
            else => return error.UnsupportedNode,
        }
    }

    fn compileBinary(self: *Compiler, binary: *const ast.Expression.Binary) CompileError!void {
        try self.compileExpression(binary.left);
        try self.compileExpression(binary.right);

        const line = binary.operator.line;
        switch (binary.operator.type) {
            .plus => try self.emitOp(OpCode.add, line),
            .minus => try self.emitOp(OpCode.subtract, line),
            .star => try self.emitOp(OpCode.multiply, line),
            .slash => try self.emitOp(OpCode.divide, line),
            .bang_equal => {
                // Keep opcodes minimal by rewriting left != right to !(left == right)
                try self.emitOp(OpCode.equal, line);
                try self.emitOp(OpCode.not, line);
            },
            .equal_equal => try self.emitOp(OpCode.equal, line),
            .greater => try self.emitOp(OpCode.greater, line),
            .greater_equal => {
                // Keep opcodes minimal by rewriting left >= right to !(left < right)
                try self.emitOp(OpCode.less, line);
                try self.emitOp(OpCode.not, line);
            },
            .less => try self.emitOp(OpCode.less, line),
            .less_equal => {
                // Keep opcodes minimal by rewriting left <= right to !(left > right)
                try self.emitOp(OpCode.greater, line);
                try self.emitOp(OpCode.not, line);
            },
            else => return error.UnsupportedNode,
        }
    }

    fn compileVariable(self: *Compiler, variable: ast.Expression.Variable) CompileError!void {
        if (self.resolveLocal(variable.token.lexeme)) |slot| {
            try self.emitOp(OpCode.get_local, variable.token.line);
            try self.emitU16(slot, variable.token.line);
        } else {
            const idx = try self.chunk.findOrAddConstant(.{ .string = variable.token.lexeme });
            try self.emitOp(OpCode.get_global, variable.token.line);
            try self.emitU16(idx, variable.token.line);
        }
    }

    fn compileVarAssignement(self: *Compiler, va: *const ast.Expression.VarAssignment) CompileError!void {
        const slot = self.resolveLocal(va.token.lexeme) orelse return error.UndefinedVariable;
        try self.compileExpression(va.value);

        try self.emitOp(OpCode.set_local, va.token.line);
        try self.emitU16(slot, va.token.line);
    }

    fn compileBlock(self: *Compiler, block: *const ast.Expression.Block) CompileError!void {
        self.beginScope();
        const body_result = try self.compileBody(block.statements);
        try self.endScope(body_result.has_result, body_result.line);
    }

    fn compileIf(self: *Compiler, if_expr: *const ast.Expression.If) CompileError!void {
        try self.compileExpression(if_expr.condition);

        // Emit the jump if condition is false with placeholder
        const else_jump = try self.emitJump(OpCode.jump_if_false, if_expr.condition.line());

        // After compiling the then_branch, we are at the bytecode position of the else_jump
        try self.compileExpression(if_expr.then_branch);

        // If condition is true, jump over the else_branch
        const end_jump = try self.emitJump(OpCode.jump, if_expr.then_branch.line());

        // Patch the else_jump with the current position
        self.patchJump(else_jump);

        if (if_expr.else_branch) |else_branch| {
            try self.compileExpression(else_branch);
        } else {
            try self.emitOp(OpCode.unit, if_expr.condition.line());
        }

        // Patch the end_jump, we know it's after the else_branch
        self.patchJump(end_jump);
    }

    fn compileFnCall(self: *Compiler, fn_call: *const ast.Expression.FnCall) CompileError!void {
        // Compile the callee and the args
        try self.compileExpression(fn_call.callee);
        for (fn_call.args) |arg| {
            try self.compileExpression(arg);
        }

        // Emit the call + arity operand
        const line = fn_call.callee.line();
        try self.emitOp(OpCode.call, line);
        try self.emitU8(@intCast(fn_call.args.len), line);
    }

    fn compileStructInit(self: *Compiler, si: *const ast.Expression.StructInit) CompileError!void {
        // Search for matching struct name
        const def_idx: u16 = blk: {
            for (self.program.struct_defs.items, 0..) |def, i| {
                if (std.mem.eql(u8, def.name, si.name.lexeme)) {
                    break :blk @intCast(i);
                }
            }
            return error.UndefinedStruct;
        };
        // Compile the arguments
        for (si.args) |arg| {
            try self.compileExpression(arg);
        }

        // Emit construct opcode + struct def index
        try self.emitOp(OpCode.construct, si.name.line);
        try self.emitU16(def_idx, si.name.line);
    }

    fn compileFieldAccess(self: *Compiler, fa: *const ast.Expression.FieldAccess) CompileError!void {
        // Access an enum variant
        if (fa.object == .variable and self.known_enum_names.contains(fa.object.variable.token.lexeme)) {
            const type_name = try self.buildEnumVariantQualifiedName(fa.object.variable.token.lexeme, fa.name.lexeme);
            defer self.allocator.free(type_name);

            // Search for a matching name
            for (self.program.struct_defs.items, 0..) |def, i| {
                if (std.mem.eql(u8, def.name, type_name)) {
                    try self.emitConstant(.{ .struct_constructor = @intCast(i) }, fa.name.line);
                    return;
                }
            }
            return error.UndefinedStruct;
        }

        // Access a field
        try self.compileExpression(fa.object);
        const name_idx = try self.chunk.findOrAddConstant(.{ .string = fa.name.lexeme });

        try self.emitOp(OpCode.get_field, fa.name.line);
        try self.emitU16(name_idx, fa.name.line);
    }

    fn compileFieldAssignment(self: *Compiler, fa: *const ast.Expression.FieldAssignment) CompileError!void {
        try self.compileExpression(fa.object);
        try self.compileExpression(fa.value);
        const name_idx = try self.chunk.findOrAddConstant(.{ .string = fa.name.lexeme });

        try self.emitOp(OpCode.set_field, fa.name.line);
        try self.emitU16(name_idx, fa.name.line);
    }

    fn compileTry(self: *Compiler, try_expr: *const ast.Expression.Try) CompileError!void {
        // Pseudo-bytecode
        // <compiler inner expression>          // [..., Result]
        // match_variant "Ok" -> err_path       // match: [..., unwrapped], no match: jump
        // jump -> done
        // err_path:                            // [..., Result]
        //   return                             // propagate Err to caller
        // done:                                // [..., unwrapped_ok_value]

        // Compile the expression
        try self.compileExpression(try_expr.expression);

        // Emit the match_variant "Ok" err_path (placeholder, future fail_jump address)
        const line = try_expr.expression.line();
        try self.emitOp(.match_variant, line);
        const name_idx = try self.chunk.findOrAddConstant(.{ .string = types.RESULT_OK_VARIANT });
        try self.emitU16(name_idx, line);

        try self.emitU16(0xFFFF, line); // Placeholder for later backpatching
        const fail_jump_offset = self.chunk.code.items.len - 2;

        // Now the jump with done (placeholder)
        const end_jump = try self.emitJump(OpCode.jump, line);
        self.patchJump(fail_jump_offset);

        // Emit the return for the err_path
        try self.emitOp(.@"return", line);
        self.patchJump(end_jump);
    }

    fn compileCatch(self: *Compiler, catch_expr: *const ast.Expression.Catch) CompileError!void {
        // Pseudo-bytecode
        // <compile expression>              // [..., Result]
        // match_variant "Ok" → handler      // match: [..., unwrapped], no match: jump
        // jump → done
        // handler:                          // [..., Result] (the Err, untouched)
        //   get_field "err"                 // [..., err_inner_value]
        //   [if binding: declare local]
        //   <compile handler expression>
        // done:

        // Compile the expression
        try self.compileExpression(catch_expr.expression);

        // Emit the match_variant "Ok" handler
        const line = catch_expr.expression.line();
        try self.emitOp(.match_variant, line);

        const name_idx = try self.chunk.findOrAddConstant(.{ .string = types.RESULT_OK_VARIANT });
        try self.emitU16(name_idx, line);

        try self.emitU16(0xFFFF, line); // Placeholder for later backpatching
        const handler_jump_offset = self.chunk.code.items.len - 2;

        // Now the jump with done
        const end_jump = try self.emitJump(OpCode.jump, line);
        self.patchJump(handler_jump_offset);

        // Emit the get_field "err"
        if (catch_expr.binding) |binding| {
            self.beginScope();
            // Extract the inner error
            try self.emitOp(OpCode.get_field, binding.line);
            const err_name_idx = try self.chunk.findOrAddConstant(.{ .string = types.FIELD_ERR });
            try self.emitU16(err_name_idx, binding.line);

            try self.declareLocal(binding.lexeme);
            try self.compileExpression(catch_expr.handler);

            //
            // true: handler produces a result
            try self.endScope(true, binding.line);
        } else {
            // Discard the Err wrapper
            try self.emitOp(OpCode.pop, line);
            try self.compileExpression(catch_expr.handler);
        }

        self.patchJump(end_jump);
    }

    // NOTE: -- Helpers

    pub fn emitReturn(self: *Compiler, line: usize) CompileError!void {
        try self.emitOp(OpCode.@"return", line);
    }

    fn emitOp(self: *Compiler, op: OpCode, line: usize) CompileError!void {
        try self.chunk.writeOp(op, line);
    }

    fn emitU8(self: *Compiler, value: u8, line: usize) CompileError!void {
        try self.chunk.writeByte(value, line);
    }

    fn emitU16(self: *Compiler, value: u16, line: usize) CompileError!void {
        try self.chunk.writeU16(value, line);
    }

    fn emitConstant(self: *Compiler, value: Value, line: usize) CompileError!void {
        const index = try self.chunk.findOrAddConstant(value);
        try self.emitOp(OpCode.constant, line);
        try self.emitU16(index, line);
    }

    fn emitJump(self: *Compiler, op: OpCode, line: usize) CompileError!usize {
        try self.emitOp(op, line);
        try self.emitU16(0xFFFF, line); // Placeholder for later backpatching
        return self.chunk.code.items.len - 2;
    }

    fn patchJump(self: *Compiler, offset: usize) void {
        // The target is the offset *after* the branch.
        // That's why it's len and not len - 1
        const target: u16 = @intCast(self.chunk.code.items.len);
        self.chunk.code.items[offset] = @truncate(target >> 8); // high
        self.chunk.code.items[offset + 1] = @truncate(target); // low
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    // Pop all variables of the current scope depth
    // If has_result is true, the last variable is the result of the scope.
    // It is kept on the stack to be handled by the caller
    fn endScope(self: *Compiler, has_result: bool, line: usize) CompileError!void {
        self.scope_depth -= 1;
        const locals_count = self.locals.items.len;

        // If we have a result and there are local variables, the stack will look like:
        // [..., local1, local2, result]
        // Because we produce a result, the stack must be:
        // [..., result]
        //
        // To achieve this, we need to overwrite local1 with result and NOT pop result
        if (has_result and locals_count > 0) {
            // Find the first local of the current scope
            const first_local_slot = blk: {
                var i = locals_count;
                // Walk backwards past all the locals in this scope to find where it starts
                while (i > 0 and self.locals.items[i - 1].depth > self.scope_depth) {
                    i -= 1;
                }
                break :blk i;
            };

            // Count of the local variables of the current scope
            const scope_locals_count = locals_count - first_local_slot;

            if (scope_locals_count > 0) {
                // Here's the magic: overwrite the first slot with the result
                try self.emitOp(OpCode.set_local, line);
                try self.emitU16(@intCast(first_local_slot), line);

                // Pop the remaining local variables, [0..scope_locals_count[,
                // so the patched result is not popped
                for (0..scope_locals_count) |_| {
                    _ = self.locals.pop();
                    try self.emitOp(OpCode.pop, line);
                }
            }
        } else {
            // Remove all locals that are out of scope
            while (self.locals.items.len > 0 and self.locals.getLast().depth > self.scope_depth) {
                _ = self.locals.pop();
                try self.emitOp(OpCode.pop, line);
            }
        }
    }

    fn declareLocal(self: *Compiler, name: []const u8) CompileError!void {
        if (self.locals.items.len >= std.math.maxInt(u16)) {
            return error.TooManyLocals;
        }

        try self.locals.append(self.allocator, .{
            .name = name,
            .depth = self.scope_depth,
        });
    }

    fn resolveLocal(self: *Compiler, name: []const u8) ?u16 {
        var i = self.locals.items.len;
        while (i > 0) {
            i -= 1;
            const local = self.locals.items[i];
            if (std.mem.eql(u8, local.name, name)) {
                return @intCast(i);
            }
        }

        return null;
    }

    // Returns the qualified name EnumName.VariantName
    fn buildEnumVariantQualifiedName(
        self: *Compiler,
        enum_name: []const u8,
        variant_name: []const u8,
    ) CompileError![]const u8 {
        const type_name = try self.allocator.alloc(u8, enum_name.len + variant_name.len + 1);
        @memcpy(type_name[0..enum_name.len], enum_name);
        type_name[enum_name.len] = '.';
        @memcpy(type_name[enum_name.len + 1 ..], variant_name);

        return type_name;
    }

    // Compiles a block of statement and leaves the last statement's result on the stack
    fn compileBody(self: *Compiler, statements: []const ast.Statement) CompileError!BodyResult {
        var has_result = false;

        // Because we will support braceless conditions,
        // it's simpler to store the last statement as the end of the block
        var line: usize = 0;
        for (statements, 0..) |statement, i| {
            // If the last statement is an expression, we must *not* pop its result
            // Compile it directly and let endScope handle it
            if (i == statements.len - 1 and statement == .expression) {
                try self.compileExpression(statement.expression);
                has_result = true;
            } else {
                try self.compileStatement(statement);
            }
            line = ast.statementLine(statement);
        }

        return BodyResult{
            .has_result = has_result,
            .line = line,
        };
    }

    const BodyResult = struct {
        has_result: bool,
        line: usize,
    };
};
