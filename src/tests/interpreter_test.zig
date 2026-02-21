const std = @import("std");
const helpers = @import("helpers");

fn expectEval(cases: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    inline for (cases) |case| {
        var buf: [64]u8 = undefined;
        var eval = try helpers.evaluate(case[0], arena.allocator());
        defer eval.deinit();
        const actual = try std.fmt.bufPrint(&buf, "{f}", .{eval.value});
        try std.testing.expectEqualStrings(case[1], actual);
    }
}

test "arithmetic" {
    try expectEval(.{
        .{ "3 + 2 * 4;", "11" },
        .{ "10 - 3;", "7" },
        .{ "6 / 2;", "3" },
    });
}

test "comparison" {
    try expectEval(.{
        .{ "3 + 2 > 4;", "true" },
        .{ "3 + 2 == 4;", "false" },
        .{ "3 + 2 == 5;", "true" },
        .{ "1 != 2;", "true" },
        .{ "3 <= 3;", "true" },
        .{ "3 >= 4;", "false" },
    });
}

test "unary" {
    try expectEval(.{
        .{ "-5;", "-5" },
        .{ "--5;", "5" },
        .{ "-(3 + 2);", "-5" },
        .{ "!0;", "true" },
        .{ "!1;", "false" },
        .{ "!true;", "false" },
        .{ "!false;", "true" },
    });
}

test "variables" {
    try expectEval(.{
        .{ "var a: Int = 1; a = 5; a;", "5" },
        .{ "var a = 1; a = 5; a;", "5" }, // Type inference
        .{ "var a = 1; a = a + 2; a;", "3" },
    });
}

test "block expressions" {
    try expectEval(.{
        .{ "{ 5; }", "5" },
        .{ "{ var a = 5; a; }", "5" },
        .{ "{ var a = 3; var b = 2; a + b; }", "5" },
    });
}

test "if expressions" {
    try expectEval(.{
        .{ "var a = if (true) { 5; } else { -5; }; a;", "5" },
        .{ "var a = if (false) { 5; } else { -5; }; a;", "-5" },
    });
}

test "function declaration and call" {
    try expectEval(.{
        .{ "fn add(a: Int, b: Int) Int { a + b; } add(1, 2);", "3" },
        .{ "fn double(x: Int) Int { x * 2; } double(5);", "10" },
        .{ "fn greet() Int { 42; } greet();", "42" },
    });
}

fn expectOutput(cases: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    inline for (cases) |case| {
        var eval = try helpers.evaluate(case[0], arena.allocator());
        defer eval.deinit();
        try std.testing.expectEqualStrings(case[1], eval.output);
    }
}

test "print" {
    try expectOutput(.{
        .{ "print(42);", "42\n" },
        .{ "print(\"hello\");", "hello\n" },
        .{ "print(1, 2, 3);", "1 2 3\n" },
        .{ "print(\"hello\", \"world\");", "hello world\n" },
    });
}

test "return statement" {
    try expectEval(.{
        .{ "fn five() Int { return 5; } five();", "5" },
        .{ "fn add(a: Int, b: Int) Int { return a + b; } add(1, 2);", "3" },
        .{ "fn early(x: Int) Int { if (x > 0) { return x; } else { return 0; } } early(5);", "5" },
        .{ "fn early(x: Int) Int { if (x > 0) { return x; } else { return 0; } } early(-1);", "0" },
    });
}

test "function with closure" {
    try expectEval(.{
        .{ "var x = 10; fn addX(a: Int) Int { a + x; } addX(5);", "15" },
    });
}
