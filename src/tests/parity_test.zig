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
