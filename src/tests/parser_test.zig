const std = @import("std");
const helpers = @import("helpers.zig");
const Parser = @import("pipe").Lexer;
const Token = @import("pipe").Token;

test "parse term" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("3 + 2;", allocator);

    const bin = result[0].expression.binary;
    try std.testing.expectEqual(.plus, bin.operator.type);
    try std.testing.expectEqual(@as(i64, 3), bin.left.literal.value.int);
    try std.testing.expectEqual(@as(i64, 2), bin.right.literal.value.int);
}

test "parse factor" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("3 + 2 * 4;", allocator);

    const bin = result[0].expression.binary;
    try std.testing.expectEqual(.plus, bin.operator.type);
    try std.testing.expectEqual(@as(i64, 3), bin.left.literal.value.int);

    const right = bin.right.binary;
    try std.testing.expectEqual(.star, right.operator.type);
    try std.testing.expectEqual(@as(i64, 2), right.left.literal.value.int);
    try std.testing.expectEqual(@as(i64, 4), right.right.literal.value.int);
}

test "parse comparison and equality precedence" {
    // 3 > 2 == 4  =>  (3 > 2) == 4
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("3 > 2 == 4;", allocator);

    const eq = result[0].expression.binary;
    try std.testing.expectEqual(.equal_equal, eq.operator.type);

    // left side: 3 > 2
    const cmp = eq.left.binary;
    try std.testing.expectEqual(.greater, cmp.operator.type);
    try std.testing.expectEqual(@as(i64, 3), cmp.left.literal.value.int);
    try std.testing.expectEqual(@as(i64, 2), cmp.right.literal.value.int);

    // right side: 4
    try std.testing.expectEqual(@as(i64, 4), eq.right.literal.value.int);
}

test "parse struct declaration" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("struct Session(const user: Int, var token: Int);", allocator);

    const decl = result[0].struct_declaration;
    try std.testing.expectEqualStrings("Session", decl.name.lexeme);
    try std.testing.expectEqual(.plain, decl.kind);
    try std.testing.expectEqual(@as(usize, 2), decl.fields.len);
    try std.testing.expectEqualStrings("user", decl.fields[0].name.lexeme);
    try std.testing.expectEqual(.constant, decl.fields[0].mutability);
    try std.testing.expectEqualStrings("token", decl.fields[1].name.lexeme);
    try std.testing.expectEqual(.mutable, decl.fields[1].mutability);
}

test "parse case struct declaration" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("case struct User(const id: Int, const name: String);", allocator);

    const decl = result[0].struct_declaration;
    try std.testing.expectEqualStrings("User", decl.name.lexeme);
    try std.testing.expectEqual(.case, decl.kind);
    try std.testing.expectEqual(@as(usize, 2), decl.fields.len);
}

test "parse dot access" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("user.name;", allocator);

    const fa = result[0].expression.field_access;
    try std.testing.expectEqualStrings("user", fa.object.variable.token.lexeme);
    try std.testing.expectEqualStrings("name", fa.name.lexeme);
}

test "parse chained dot access" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const result = try helpers.parse("a.b.c;", allocator);

    const outer = result[0].expression.field_access;
    try std.testing.expectEqualStrings("c", outer.name.lexeme);
    const inner = outer.object.field_access;
    try std.testing.expectEqualStrings("b", inner.name.lexeme);
    try std.testing.expectEqualStrings("a", inner.object.variable.token.lexeme);
}
