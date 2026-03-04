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
    dot,
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
    pipe,

    // Literals
    identifier,
    int,
    string,

    // Keywords
    @"and",
    case,
    @"catch",
    @"const",
    @"else",
    @"error",
    false,
    @"fn",
    @"for",
    @"if",
    null,
    @"or",
    @"return",
    self_type,
    @"struct",
    true,
    @"try",
    @"enum",
    @"var",
    when,
    @"while",

    eof,

    pub fn keyword(bytes: []const u8) ?TokenType {
        return keywords.get(bytes);
    }

    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        .{ "and", .@"and" },
        .{ "case", .case },
        .{ "catch", .@"catch" },
        .{ "const", .@"const" },
        .{ "else", .@"else" },
        .{ "error", .@"error" },
        .{ "false", .false },
        .{ "fn", .@"fn" },
        .{ "for", .@"for" },
        .{ "if", .@"if" },
        .{ "null", .null },
        .{ "or", .@"or" },
        .{ "Self", .self_type },
        .{ "struct", .@"struct" },
        .{ "return", .@"return" },
        .{ "true", .true },
        .{ "enum", .@"enum" },
        .{ "try", .@"try" },
        .{ "var", .@"var" },
        .{ "while", .@"while" },
        .{ "when", .when },
    });
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
};
