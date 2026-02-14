const std = @import("std");
const helpers = @import("helpers");

fn expectEval(cases: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    inline for (cases) |case| {
        var buf: [64]u8 = undefined;
        const result = try helpers.evaluate(case[0], arena.allocator());
        const actual = try std.fmt.bufPrint(&buf, "{f}", .{result});
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
        .{ "var a = 1; a = 5; a;", "5" },
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
        .{ "fn add(a, b) { a + b; } add(1, 2);", "3" },
        .{ "fn double(x) { x * 2; } double(5);", "10" },
        .{ "fn greet() { 42; } greet();", "42" },
    });
}

test "function with closure" {
    try expectEval(.{
        .{ "var x = 10; fn addX(a) { a + x; } addX(5);", "15" },
    });
}
