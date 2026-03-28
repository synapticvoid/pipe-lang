const std = @import("std");
const helpers = @import("helpers");

fn expectParity(cases: anytype) !void {
    inline for (cases) |source| {
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        var interp = try helpers.evaluate(source, allocator);
        defer interp.deinit();

        var vm = try helpers.evaluateVm(source, allocator);
        defer vm.deinit();

        var interp_buf: [128]u8 = undefined;
        var vm_buf: [128]u8 = undefined;
        const interp_str = try std.fmt.bufPrint(&interp_buf, "{f}", .{interp.value});
        const vm_str = try std.fmt.bufPrint(&vm_buf, "{f}", .{vm.value});

        std.testing.expectEqualStrings(interp_str, vm_str) catch |err| {
            std.debug.print("\nParity failure for: {s}\n  interpreter: {s}\n  vm:          {s}\n", .{ source, interp_str, vm_str });
            return err;
        };
    }
}

test "parity: arithmetic" {
    try expectParity(.{
        "3 + 2 * 4;",
        "10 - 3;",
        "6 / 2;",
    });
}

test "parity: comparison" {
    try expectParity(.{
        "3 + 2 > 4;",
        "3 + 2 == 4;",
        "3 + 2 == 5;",
        "1 != 2;",
        "3 <= 3;",
        "3 >= 4;",
    });
}

test "parity: unary" {
    try expectParity(.{
        "-5;",
        "--5;",
        "-(3 + 2);",
        "!0;",
        "!1;",
        "!true;",
        "!false;",
    });
}

test "parity: variables" {
    try expectParity(.{
        "var a = 1; a = 5; a;",
        "var a = 1; a = a + 2; a;",
    });
}

test "parity: block expressions" {
    try expectParity(.{
        "{ 5; }",
        "{ var a = 5; a; }",
        "{ var a = 3; var b = 2; a + b; }",
    });
}

test "parity: if expressions" {
    try expectParity(.{
        "var a = if true { 5; } else { -5; }; a;",
        "var a = if false { 5; } else { -5; }; a;",
    });
}

test "parity: function call with return" {
    try expectParity(.{
        "fn five() Int { return 5; } five();",
        "fn add(a: Int, b: Int) Int { return a + b; } add(1, 2);",
    });
}

test "parity: nested function calls" {
    try expectParity(.{
        "fn double(x: Int) Int { return x * 2; } fn quad(x: Int) Int { return double(double(x)); } quad(3);",
    });
}

test "parity: function preserves caller locals" {
    try expectParity(.{
        "var a = 10; fn add(x: Int, y: Int) Int { return x + y; } var b = add(3, 5); a + b;",
    });
}

test "parity: recursion" {
    try expectParity(.{
        "fn fact(n: Int) Int { if n <= 1 { return 1; } else { return n * fact(n - 1); } } fact(5);",
    });
}

fn expectOutputParity(cases: anytype) !void {
    inline for (cases) |case| {
        const source = case[0];
        var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        var interp = try helpers.evaluate(source, allocator);
        defer interp.deinit();

        var vm = try helpers.evaluateVm(source, allocator);
        defer vm.deinit();

        std.testing.expectEqualStrings(interp.output, vm.output) catch |err| {
            std.debug.print("\nOutput parity failure for: {s}\n  interpreter: {s}\n  vm:          {s}\n", .{ source, interp.output, vm.output });
            return err;
        };

        // Also verify against the expected output if provided
        const expected = case[1];
        std.testing.expectEqualStrings(expected, vm.output) catch |err| {
            std.debug.print("\nExpected output failure for: {s}\n  expected: {s}\n  got:      {s}\n", .{ source, expected, vm.output });
            return err;
        };
    }
}

test "parity: print integer" {
    try expectOutputParity(.{
        .{ "print(42);", "42\n" },
    });
}

test "parity: print string" {
    try expectOutputParity(.{
        .{ "print(\"hello\");", "hello\n" },
    });
}

test "parity: print multiple args" {
    try expectOutputParity(.{
        .{ "print(1, 2, 3);", "1 2 3\n" },
    });
}

test "parity: print boolean" {
    try expectOutputParity(.{
        .{ "print(true);", "true\n" },
    });
}
