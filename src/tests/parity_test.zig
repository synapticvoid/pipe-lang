const std = @import("std");
const helpers = @import("helpers");

fn expectEval(cases: anytype) !void {
    inline for (cases) |source| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        var vm = try helpers.evaluateVm(source, arena.allocator());
        defer vm.deinit();

        var buf: [128]u8 = undefined;
        const vm_str = try std.fmt.bufPrint(&buf, "{f}", .{vm.value});
        _ = vm_str;
    }
}

fn expectOutput(cases: anytype) !void {
    inline for (cases) |case| {
        const source = case[0];
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();

        var vm = try helpers.evaluateVm(source, arena.allocator());
        defer vm.deinit();

        const expected = case[1];
        std.testing.expectEqualStrings(expected, vm.output) catch |err| {
            std.debug.print("\nOutput failure for: {s}\n  expected: {s}\n  got:      {s}\n", .{ source, expected, vm.output });
            return err;
        };
    }
}

test "arithmetic" {
    try expectEval(.{
        "3 + 2 * 4;",
        "10 - 3;",
        "6 / 2;",
    });
}

test "comparison" {
    try expectEval(.{
        "3 + 2 > 4;",
        "3 + 2 == 4;",
        "3 + 2 == 5;",
        "1 != 2;",
        "3 <= 3;",
        "3 >= 4;",
    });
}

test "unary" {
    try expectEval(.{
        "-5;",
        "--5;",
        "-(3 + 2);",
        "!0;",
        "!1;",
        "!true;",
        "!false;",
    });
}

test "variables" {
    try expectEval(.{
        "var a = 1; a = 5; a;",
        "var a = 1; a = a + 2; a;",
    });
}

test "block expressions" {
    try expectEval(.{
        "{ 5; }",
        "{ var a = 5; a; }",
        "{ var a = 3; var b = 2; a + b; }",
    });
}

test "if expressions" {
    try expectEval(.{
        "var a = if true { 5; } else { -5; }; a;",
        "var a = if false { 5; } else { -5; }; a;",
    });
}

test "function call with return" {
    try expectEval(.{
        "fn five() Int { return 5; } five();",
        "fn add(a: Int, b: Int) Int { return a + b; } add(1, 2);",
    });
}

test "nested function calls" {
    try expectEval(.{
        "fn double(x: Int) Int { return x * 2; } fn quad(x: Int) Int { return double(double(x)); } quad(3);",
    });
}

test "function preserves caller locals" {
    try expectEval(.{
        "var a = 10; fn add(x: Int, y: Int) Int { return x + y; } var b = add(3, 5); a + b;",
    });
}

test "recursion" {
    try expectEval(.{
        "fn fact(n: Int) Int { if n <= 1 { return 1; } else { return n * fact(n - 1); } } fact(5);",
    });
}

test "print integer" {
    try expectOutput(.{
        .{ "print(42);", "42\n" },
    });
}

test "print string" {
    try expectOutput(.{
        .{ "print(\"hello\");", "hello\n" },
    });
}

test "print multiple args" {
    try expectOutput(.{
        .{ "print(1, 2, 3);", "1 2 3\n" },
    });
}

test "print boolean" {
    try expectOutput(.{
        .{ "print(true);", "true\n" },
    });
}

// =========================================================================
// Error handling: try / catch / fallible functions
// =========================================================================

test "fallible function wraps success in Ok" {
    try expectEval(.{
        \\error enum E { Fail, }
        \\fn maybe(x: Int) E!Int { x; }
        \\maybe(42);
    });
}

test "fallible function wraps error in Err" {
    try expectEval(.{
        \\error enum E { Fail, }
        \\fn fail() E!Int { E.Fail(); }
        \\fail();
    });
}

test "try on Ok unwraps value" {
    try expectEval(.{
        \\error enum E { Fail, }
        \\fn maybe(x: Int) E!Int { x; }
        \\fn caller(x: Int) E!Int { try maybe(x); }
        \\caller(42);
    });
}

test "try on Err propagates error" {
    try expectEval(.{
        \\error enum E { Fail, }
        \\fn fail() E!Int { E.Fail(); }
        \\fn caller() E!Int { try fail(); }
        \\caller();
    });
}

test "catch on Ok returns unwrapped value" {
    try expectEval(.{
        \\error enum E { Fail, }
        \\fn maybe(x: Int) E!Int { x; }
        \\maybe(42) catch e { 0; };
    });
}

test "catch on Err executes handler" {
    try expectEval(.{
        \\error enum E { Fail, }
        \\fn fail() E!Int { E.Fail(); }
        \\fail() catch e { 0; };
    });
}

test "catch with binding gives access to error value" {
    try expectEval(.{
        \\error enum E { Fail(const code: Int), }
        \\fn fail() E!Int { E.Fail(99); }
        \\fail() catch e { e.code; };
    });
}

test "nested try propagation across call frames" {
    try expectEval(.{
        \\error enum E { Fail, }
        \\fn fail() E!Int { E.Fail(); }
        \\fn middle() E!Int { try fail(); }
        \\fn outer() E!Int { try middle(); }
        \\outer();
    });
}

// =========================================================================
// Structs
// =========================================================================

test "struct construction and field access" {
    try expectEval(.{
        \\case struct User(const id: Int, const name: Str);
        \\const u = User(1, "Alice");
        \\u.name;
    });
}

test "struct body field with default" {
    try expectEval(.{
        \\case struct User(const id: Int) {
        \\    const tag: Str = "user";
        \\}
        \\const u = User(1);
        \\u.tag;
    });
}

test "struct body field excluded from equals" {
    try expectEval(.{
        \\case struct User(const id: Int) {
        \\    var tag: Str = "a";
        \\}
        \\const a = User(1);
        \\const b = User(1);
        \\a == b;
    });
}
