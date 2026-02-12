const std = @import("std");
const helpers = @import("helpers.zig");
const Parser = @import("pipe").Lexer;
const Token = @import("pipe").Token;

test "parse term" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("3 + 2", allocator);

    const bin = result[0].expression.binary;
    try std.testing.expectEqual(.plus, bin.operator.type);
    try std.testing.expectEqual(3.0, bin.left.literal.value.number);
    try std.testing.expectEqual(2.0, bin.right.literal.value.number);
}

test "parse factor" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("3 + 2 * 4", allocator);

    const bin = result[0].expression.binary;
    try std.testing.expectEqual(.plus, bin.operator.type);
    try std.testing.expectEqual(3.0, bin.left.literal.value.number);

    const right = bin.right.binary;
    try std.testing.expectEqual(.star, right.operator.type);
    try std.testing.expectEqual(2.0, right.left.literal.value.number);
    try std.testing.expectEqual(4.0, right.right.literal.value.number);
}
