const std = @import("std");
const helpers = @import("helpers");

test "math operations" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const cases = .{
        // arithmetic
        .{ "3 + 2 * 4;", "11" },
        .{ "10 - 3;", "7" },
        .{ "6 / 2;", "3" },

        // comparison
        .{ "3 + 2 > 4;", "true" },
        .{ "3 + 2 == 4;", "false" },
        .{ "3 + 2 == 5;", "true" },
        .{ "1 != 2;", "true" },
        .{ "3 <= 3;", "true" },
        .{ "3 >= 4;", "false" },

        // unary
        .{ "-5;", "-5" },
        .{ "--5;", "5" },
        .{ "-(3 + 2);", "-5" },
        .{ "!0;", "true" },
        .{ "!1;", "false" },

        // boolean
        .{ "!true;", "false" },
        .{ "!false;", "true" },

        // variables
        .{ "var a = 1; a = 5; a;", "5" },
        .{ "var a = 1; a = a + 2; a;", "3" },
    };

    inline for (cases) |case| {
        var buf: [64]u8 = undefined;
        const result = try helpers.evaluate(case[0], arena.allocator());
        const actual = try std.fmt.bufPrint(&buf, "{f}", .{result});
        try std.testing.expectEqualStrings(case[1], actual);
    }
}
