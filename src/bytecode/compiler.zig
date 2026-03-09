const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const ChunkError = @import("chunk.zig").ChunkError;
const OpCode = @import("opcode.zig").OpCode;
const ast = @import("../ast.zig");
const Value = ast.Value;

const Local = struct {
    name: []const u8,
    // nesting depth at which this local was declared
    depth: u32,
};

pub const CompileError = error{
    UnsupportedNode,
    TooManyLocals,
} || ChunkError;

pub const Compiler = struct {
    chunk: *Chunk,
    locals: std.ArrayList(Local),
    scope_depth: u32,
    allocator: std.mem.Allocator,

    pub fn init(chunk: *Chunk, allocator: std.mem.Allocator) Compiler {
        return .{
            .chunk = chunk,
            .locals = .{},
            .scope_depth = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.locals.deinit(self.allocator);
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
            else => return error.UnsupportedNode,
        }
    }

    fn compileVarDeclarationStatement(self: *Compiler, var_decl: ast.Statement.VarDeclaration) CompileError!void {
        _ = self;
        _ = var_decl;
    }

    // NOTE: -- Expressions

    fn compileExpression(self: *Compiler, expr: ast.Expression) CompileError!void {
        _ = self;
        _ = expr;
    }

    // NOTE: -- Helpers

    fn emitOp(self: *Compiler, op: OpCode, line: u32) CompileError!void {
        try self.chunk.writeOp(op, line);
    }

    fn emitU16(self: *Compiler, value: u16, line: u32) CompileError!void {
        try self.chunk.writeU16(value, line);
    }

    fn emitConstant(self: *Compiler, value: Value, line: u32) CompileError!void {
        const index = try self.chunk.addConstant(value);
        try self.emitOp(OpCode.constant, line);
        try self.emitU16(index, line);
    }

    fn emitJump(self: *Compiler, op: OpCode, line: u32) CompileError!usize {
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

    fn endScope(self: *Compiler, line: u32) CompileError!void {
        self.scope_depth -= 1;
        // Remove all locals that are out of scope
        while (self.locals.items.len > 0 and self.locals.getLast().depth > self.scope_depth) {
            _ = self.locals.pop();
            try self.emitOp(OpCode.pop, line);
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
};
