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

// -- Error type declarations

test "error declaration is valid" {
    try expectTypeCheck("error MathError { DivByZero, Overflow }");
}

test "error union declaration is valid" {
    try expectTypeCheck("error MathError { DivByZero } error AppError = MathError | MathError");
}

test "explicit error union with unknown error set is rejected" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck("fn f() Unknown!Int { 1; }", arena.allocator());
    try std.testing.expectError(error.UndefinedType, result);
}

// -- Fallible functions

test "fallible function with ok return is valid" {
    try expectTypeCheck(
        \\error MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
    );
}

test "inferred error union is valid" {
    try expectTypeCheck(
        \\fn mayFail(x: Int) !Int { x; }
    );
}

// -- try

test "try inside fallible function is valid" {
    try expectTypeCheck(
        \\error MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) !Int { try divide(a, b); }
    );
}

test "try outside fallible function is rejected" {
    try expectTypeError(
        \\error MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) Int { try divide(a, b); }
    );
}

test "try on non-fallible expression is rejected" {
    try expectTypeError(
        \\fn add(a: Int, b: Int) Int { a + b; }
        \\fn safe(a: Int, b: Int) !Int { try add(a, b); }
    );
}

// -- catch

test "catch on fallible expression is valid" {
    try expectTypeCheck(
        \\error MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) Int { divide(a, b) catch { -1; }; }
    );
}

test "catch with binding is valid" {
    try expectTypeCheck(
        \\error MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) Int { divide(a, b) catch |e| { -1; }; }
    );
}

test "catch on non-fallible expression is rejected" {
    try expectTypeError(
        \\fn add(a: Int, b: Int) Int { a + b; }
        \\fn safe(a: Int, b: Int) Int { add(a, b) catch { 0; }; }
    );
}
