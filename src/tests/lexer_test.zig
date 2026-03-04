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

test "tokenize fat arrow" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("e => recover(e)", allocator);
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    const expected = [_]Token{
        .{ .type = .identifier, .lexeme = "e", .line = 1 },
        .{ .type = .fat_arrow, .lexeme = "=>", .line = 1 },
        .{ .type = .identifier, .lexeme = "recover", .line = 1 },
        .{ .type = .lparen, .lexeme = "(", .line = 1 },
        .{ .type = .identifier, .lexeme = "e", .line = 1 },
        .{ .type = .rparen, .lexeme = ")", .line = 1 },
        .{ .type = .eof, .lexeme = "", .line = 1 },
    };

    try std.testing.expectEqualDeep(&expected, tokens);
}

test "tokenize type system keywords" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("struct enum case when self Self", allocator);
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    const expected = [_]Token{
        .{ .type = .@"struct", .lexeme = "struct", .line = 1 },
        .{ .type = .@"enum", .lexeme = "enum", .line = 1 },
        .{ .type = .case, .lexeme = "case", .line = 1 },
        .{ .type = .when, .lexeme = "when", .line = 1 },
        .{ .type = .identifier, .lexeme = "self", .line = 1 },
        .{ .type = .self_type, .lexeme = "Self", .line = 1 },
        .{ .type = .eof, .lexeme = "", .line = 1 },
    };

    try std.testing.expectEqualDeep(&expected, tokens);
}

test "tokenize dot access" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("user.name", allocator);
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    const expected = [_]Token{
        .{ .type = .identifier, .lexeme = "user", .line = 1 },
        .{ .type = .dot, .lexeme = ".", .line = 1 },
        .{ .type = .identifier, .lexeme = "name", .line = 1 },
        .{ .type = .eof, .lexeme = "", .line = 1 },
    };

    try std.testing.expectEqualDeep(&expected, tokens);
}

test "tokenize case struct declaration" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init("case struct User(const id: Int);", allocator);
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    const expected = [_]Token{
        .{ .type = .case, .lexeme = "case", .line = 1 },
        .{ .type = .@"struct", .lexeme = "struct", .line = 1 },
        .{ .type = .identifier, .lexeme = "User", .line = 1 },
        .{ .type = .lparen, .lexeme = "(", .line = 1 },
        .{ .type = .@"const", .lexeme = "const", .line = 1 },
        .{ .type = .identifier, .lexeme = "id", .line = 1 },
        .{ .type = .colon, .lexeme = ":", .line = 1 },
        .{ .type = .identifier, .lexeme = "Int", .line = 1 },
        .{ .type = .rparen, .lexeme = ")", .line = 1 },
        .{ .type = .semicolon, .lexeme = ";", .line = 1 },
        .{ .type = .eof, .lexeme = "", .line = 1 },
    };

    try std.testing.expectEqualDeep(&expected, tokens);
}
