const std = @import("std");
const helpers = @import("helpers");

fn expectTypeCheck(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    try helpers.typeCheck(source, arena.allocator());
}

fn expectTypeError(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck(source, arena.allocator());
    try std.testing.expectError(error.TypeMismatch, result);
}

fn expectConstError(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck(source, arena.allocator());
    try std.testing.expectError(error.ConstReassignment, result);
}

test "return type matches declaration" {
    try expectTypeCheck("fn five() Int { return 5; }");
    try expectTypeCheck("fn greet() { return; }");
    try expectTypeCheck("fn greet() Unit { return; }");
    try expectTypeCheck("fn add(a: Int, b: Int) Int { return a + b; }");
}

test "return type mismatch" {
    try expectTypeError("fn five() Int { return true; }");
    try expectTypeError("fn flag() Bool { return 42; }");
}

test "implicit return type checked" {
    try expectTypeCheck("fn five() Int { 5; }");
    try expectTypeError("fn five() Int { true; }");
}

test "const reassignment rejected" {
    try expectConstError("const x = 5; x = 10;");
}

test "var reassignment allowed" {
    try expectTypeCheck("var x = 5; x = 10;");
}

test "var type mismatch on reassignment" {
    try expectTypeError("var x = 5; x = true;");
}
