const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;

/// Prints a full chunk: header followed by all instructions.
/// Output example:
///   === test_chunk ===
///   0000    1 OP_CONSTANT    0 '42'
///   0003    | OP_RETURN
pub fn disassembleChunk(writer: anytype, chunk: *const Chunk, name: []const u8) !void {
    try writer.print("=== {s} ===\n", .{name});
    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = try disassembleInstruction(writer, chunk, offset);
    }
}

/// Prints a single instruction and returns the next byte offset.
/// Output examples:
///   0000    1 OP_ADD
///   0001    | OP_RETURN
///   0002    2 OP_CONSTANT    0 '42'
///   0005    3 OP_JUMP        0012
pub fn disassembleInstruction(writer: anytype, chunk: *const Chunk, offset: usize) !usize {
    if (offset >= chunk.code.items.len) {
        return error.InvalidOffset;
    }

    // Print offset
    try writer.print("{d:0>4} ", .{offset});

    // Print line with repetition marker
    const line = chunk.lines.items[offset];
    if (offset > 0 and chunk.lines.items[offset - 1] == line) {
        try writer.print("   | ", .{});
    } else {
        try writer.print("{d:0>4} ", .{line});
    }

    // Print opcode + operand
    const op: OpCode = @enumFromInt(chunk.code.items[offset]);
    switch (op) {
        // No operand
        .add,
        .subtract,
        .multiply,
        .divide,
        .negate,
        .not,
        .equal,
        .greater,
        .less,
        .true,
        .false,
        .null,
        .pop,
        .print,
        .@"return",
        => {
            try writer.print("{s}\n", .{@tagName(op)});
            return offset + 1;
        },

        // u16 operand (pool index)
        .constant,
        .get_global,
        .set_global,
        .define_global,
        => {
            if (offset + 2 >= chunk.code.items.len) {
                return error.InvalidOffset;
            }
            //
            // Read u16, print name + index + constant value
            const pool_index = @as(u16, chunk.code.items[offset + 1]) << 8 | chunk.code.items[offset + 2];
            const constant = chunk.constants.items[pool_index];

            try writer.print("{s} {d} '{any}'\n", .{ @tagName(op), pool_index, constant });

            return offset + 3;
        },

        // u16 index (stack slot index)
        .jump,
        .jump_if_false,
        .loop,
        .get_local,
        .set_local,
        => {
            if (offset + 2 >= chunk.code.items.len) {
                return error.InvalidOffset;
            }

            // u16, print name + raw u16 operand
            const operand = @as(u16, chunk.code.items[offset + 1]) << 8 | chunk.code.items[offset + 2];

            try writer.print("{s} {d}\n", .{ @tagName(op), operand });
            return offset + 3;
        },
    }
}
