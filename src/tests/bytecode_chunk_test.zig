const std = @import("std");
const vm_pkg = @import("pipe").vm;
const Chunk = vm_pkg.Chunk;
const OpCode = vm_pkg.OpCode;

test "code and lines arrays stay in sync" {
    const allocator = std.testing.allocator;
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.writeOp(.add, 1);
    try chunk.writeByte(0xFF, 1);
    try chunk.writeU16(0x1234, 2);

    try std.testing.expectEqual(chunk.code.items.len, chunk.lines.items.len);
}

test "writeU16 encodes big-endian" {
    const allocator = std.testing.allocator;
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.writeU16(0xABCD, 1);

    try std.testing.expectEqual(@as(u8, 0xAB), chunk.code.items[0]);
    try std.testing.expectEqual(@as(u8, 0xCD), chunk.code.items[1]);
}

test "addConstant returns sequential indices" {
    const allocator = std.testing.allocator;
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    const idx0 = try chunk.addConstant(.{ .int = 42 });
    const idx1 = try chunk.addConstant(.{ .int = 99 });

    try std.testing.expectEqual(@as(u16, 0), idx0);
    try std.testing.expectEqual(@as(u16, 1), idx1);
}

test "getLine returns correct line for offset" {
    const allocator = std.testing.allocator;
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.writeOp(.add, 10);
    try chunk.writeOp(.subtract, 20);
    try chunk.writeOp(.@"return", 30);

    try std.testing.expectEqual(@as(u32, 10), try chunk.getLine(0));
    try std.testing.expectEqual(@as(u32, 20), try chunk.getLine(1));
    try std.testing.expectEqual(@as(u32, 30), try chunk.getLine(2));
}

test "getLine returns error for out-of-bounds offset" {
    const allocator = std.testing.allocator;
    var chunk = Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.writeOp(.add, 1);

    try std.testing.expectError(error.InvalidOffset, chunk.getLine(99));
}
