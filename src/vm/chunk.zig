const std = @import("std");
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("../ast.zig").Value;

pub const ChunkError = error{
    InvalidOffset,
    TooManyConstants,
};

pub const Chunk = struct {
    /// Bytecode instruction stream.
    code: std.ArrayList(u8),
    /// Source line for each byte in `code` (parallel array).
    lines: std.ArrayList(usize),
    /// Constant pool referenced by operand indices.
    constants: std.ArrayList(Value),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{
            .code = .{},
            .lines = .{},
            .constants = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit(self.allocator);
        self.lines.deinit(self.allocator);
        self.constants.deinit(self.allocator);
    }

    pub fn writeByte(self: *Chunk, byte: u8, line: usize) !void {
        try self.code.append(self.allocator, byte);
        try self.lines.append(self.allocator, line);
    }

    pub fn writeOp(self: *Chunk, op: OpCode, line: usize) !void {
        try self.writeByte(@intFromEnum(op), line);
    }

    pub fn writeU16(self: *Chunk, value: u16, line: usize) !void {
        try self.writeByte(@truncate(value >> 8), line); // high bytes
        try self.writeByte(@truncate(value), line); // low bytes
    }

    // Add the constant to the pool and return its index
    pub fn addConstant(self: *Chunk, value: Value) !u16 {
        if (self.constants.items.len >= std.math.maxInt(u16)) {
            return error.TooManyConstants;
        }

        try self.constants.append(self.allocator, value);
        return @intCast(self.constants.items.len - 1);
    }

    pub fn getLine(self: *Chunk, offset: usize) !usize {
        if (offset >= self.lines.items.len) {
            return error.InvalidOffset;
        }

        return self.lines.items[offset];
    }
};
