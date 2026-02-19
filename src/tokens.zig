const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens
    lparen,
    rparen,
    lbrace,
    rbrace,
    colon,
    semicolon,
    comma,
    minus,
    plus,
    slash,
    star,

    // One or two character tokens
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

    // Literals
    identifier,
    int,
    string,

    // Keywords
    @"and",
    @"const",
    @"else",
    false,
    @"fn",
    @"for",
    @"if",
    null,
    @"or",
    print,
    @"return",
    true,
    @"var",
    @"while",

    eof,

    pub fn keyword(bytes: []const u8) ?TokenType {
        return keywords.get(bytes);
    }

    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        .{ "and", .@"and" },
        .{ "const", .@"const" },
        .{ "else", .@"else" },
        .{ "false", .false },
        .{ "fn", .@"fn" },
        .{ "for", .@"for" },
        .{ "if", .@"if" },
        .{ "null", .null },
        .{ "or", .@"or" },
        .{ "return", .@"return" },
        .{ "true", .true },
        .{ "var", .@"var" },
        .{ "while", .@"while" },
    });
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
};
