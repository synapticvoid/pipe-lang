const std = @import("std");
const Lexer = @import("pipe").Lexer;
const Token = @import("pipe").Token;

test "tokenize simple expression" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("3 + 2", allocator);
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    const expected = [_]Token{
        .{ .type = .number, .lexeme = "3", .line = 1 },
        .{ .type = .plus, .lexeme = "+", .line = 1 },
        .{ .type = .number, .lexeme = "2", .line = 1 },
        .{ .type = .eof, .lexeme = "", .line = 1 },
    };

    try std.testing.expectEqualDeep(&expected, tokens);
}