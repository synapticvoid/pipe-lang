const std = @import("std");
const vm_pkg = @import("pipe").vm;
const Chunk = vm_pkg.Chunk;
const OpCode = vm_pkg.OpCode;
const disassemble = vm_pkg.disassemble;

fn disassembleToString(chunk: *const Chunk) ![]const u8 {
    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(std.testing.allocator);
    const writer = buf.writer(std.testing.allocator);
    try disassemble.disassembleChunk(writer, chunk, "test");
    return try buf.toOwnedSlice(std.testing.allocator);
}

fn disassembleInstructionToString(chunk: *const Chunk, offset: usize) ![]const u8 {
    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(std.testing.allocator);
    const writer = buf.writer(std.testing.allocator);
    _ = try disassemble.disassembleInstruction(writer, chunk, offset);
    return try buf.toOwnedSlice(std.testing.allocator);
}

test "disassemble simple no-operand opcodes" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.add, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try disassembleInstructionToString(&chunk, 0);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("0000 0001 add\n", result);
}

test "disassemble line repetition marker" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.add, 1);
    try chunk.writeOp(.subtract, 1);

    const result = try disassembleInstructionToString(&chunk, 1);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("0001    | subtract\n", result);
}

test "disassemble constant opcode with value" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx = try chunk.addConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx, 1);

    const result = try disassembleInstructionToString(&chunk, 0);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("0000 0001 constant 0 '.{ .int = 42 }'\n", result);
}

test "disassemble jump opcode" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.jump, 5);
    try chunk.writeU16(0x0100, 5);

    const result = try disassembleInstructionToString(&chunk, 0);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("0000 0005 jump 256\n", result);
}

test "disassemble get_field opcode with constant operand" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx = try chunk.addConstant(.{ .int = 7 });
    try chunk.writeOp(.get_field, 1);
    try chunk.writeU16(idx, 1);

    const result = try disassembleInstructionToString(&chunk, 0);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("0000 0001 get_field 0 '.{ .int = 7 }'\n", result);
}

test "disassemble set_field opcode with constant operand" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx = try chunk.addConstant(.{ .int = 11 });
    try chunk.writeOp(.set_field, 1);
    try chunk.writeU16(idx, 1);

    const result = try disassembleInstructionToString(&chunk, 0);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("0000 0001 set_field 0 '.{ .int = 11 }'\n", result);
}

test "disassemble construct opcode with raw u16 operand" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.construct, 3);
    try chunk.writeU16(5, 3);

    const result = try disassembleInstructionToString(&chunk, 0);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("0000 0003 construct 5\n", result);
}

test "disassemble full chunk with header" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx = try chunk.addConstant(.{ .int = 99 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx, 1);
    try chunk.writeOp(.@"return", 2);

    const result = try disassembleToString(&chunk);
    defer std.testing.allocator.free(result);

    const expected =
        \\=== test ===
        \\0000 0001 constant 0 '.{ .int = 99 }'
        \\0003 0002 return
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "disassemble truncated operand returns error" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    // Write constant opcode but only one operand byte (missing the second)
    try chunk.writeOp(.constant, 1);
    try chunk.writeByte(0x00, 1);

    var buf: std.ArrayList(u8) = .{};
    defer buf.deinit(std.testing.allocator);
    const writer = buf.writer(std.testing.allocator);

    try std.testing.expectError(error.InvalidOffset, disassemble.disassembleInstruction(writer, &chunk, 0));
}
