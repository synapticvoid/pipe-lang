const std = @import("std");
const Lexer = @import("pipe").Lexer;
const Token = @import("pipe").Token;

test "tokenize simple expression" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("3 + 2", allocator);
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    const expected = [_]Token{
        .{ .type = .int, .lexeme = "3", .line = 1 },
        .{ .type = .plus, .lexeme = "+", .line = 1 },
        .{ .type = .int, .lexeme = "2", .line = 1 },
        .{ .type = .eof, .lexeme = "", .line = 1 },
    };

    try std.testing.expectEqualDeep(&expected, tokens);
}

test "tokenize comparison operators" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("3 >= 2 == 1 < 4 != 5 <= 6 > 7", allocator);
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    const expected = [_]Token{
        .{ .type = .int, .lexeme = "3", .line = 1 },
        .{ .type = .greater_equal, .lexeme = ">=", .line = 1 },
        .{ .type = .int, .lexeme = "2", .line = 1 },
        .{ .type = .equal_equal, .lexeme = "==", .line = 1 },
        .{ .type = .int, .lexeme = "1", .line = 1 },
        .{ .type = .less, .lexeme = "<", .line = 1 },
        .{ .type = .int, .lexeme = "4", .line = 1 },
        .{ .type = .bang_equal, .lexeme = "!=", .line = 1 },
        .{ .type = .int, .lexeme = "5", .line = 1 },
        .{ .type = .less_equal, .lexeme = "<=", .line = 1 },
        .{ .type = .int, .lexeme = "6", .line = 1 },
        .{ .type = .greater, .lexeme = ">", .line = 1 },
        .{ .type = .int, .lexeme = "7", .line = 1 },
        .{ .type = .eof, .lexeme = "", .line = 1 },
    };

    try std.testing.expectEqualDeep(&expected, tokens);
}