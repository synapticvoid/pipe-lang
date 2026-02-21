const std = @import("std");
const isDigit = std.ascii.isDigit;

const tok = @import("tokens.zig");
const Token = tok.Token;
const TokenType = tok.TokenType;

pub const Lexer = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    tokens: std.ArrayList(Token),
    allocator: std.mem.Allocator,

    // NOTE: -- Public API

    pub fn init(source: []const u8, allocator: std.mem.Allocator) Lexer {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .tokens = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit(self.allocator);
    }

    pub fn tokenize(self: *Lexer) ![]const Token {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }

        try self.tokens.append(self.allocator, .{
            .type = .eof,
            .lexeme = "",
            .line = self.line,
        });

        return self.tokens.items;
    }

    // NOTE: -- Scanning

    fn scanToken(self: *Lexer) !void {
        const c = self.advance();

        switch (c) {
            // Delimiters
            '(' => try self.addToken(.lparen, null),
            ')' => try self.addToken(.rparen, null),
            '{' => try self.addToken(.lbrace, null),
            '}' => try self.addToken(.rbrace, null),
            ':' => try self.addToken(.colon, null),
            ';' => try self.addToken(.semicolon, null),
            ',' => try self.addToken(.comma, null),

            // Single-character operators
            '+' => try self.addToken(.plus, null),
            '-' => try self.addToken(.minus, null),
            '*' => try self.addToken(.star, null),

            // One or two character operators
            '/' => {
                if (self.match('/')) {
                    // A line comment `//` goes until the end of the line.
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                    return;
                }

                try self.addToken(.slash, null);
            },
            '<' => {
                if (self.match('=')) {
                    try self.addToken(.less_equal, null);
                } else {
                    try self.addToken(.less, null);
                }
            },
            '>' => {
                if (self.match('=')) {
                    try self.addToken(.greater_equal, null);
                } else {
                    try self.addToken(.greater, null);
                }
            },
            '=' => {
                if (self.match('=')) {
                    try self.addToken(.equal_equal, null);
                } else {
                    try self.addToken(.equal, null);
                }
            },
            '!' => {
                if (self.match('=')) {
                    try self.addToken(.bang_equal, null);
                } else {
                    try self.addToken(.bang, null);
                }
            },
            '|' => {
                try self.addToken(.pipe, null);
            },

            // Whitespace
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,

            '"' => try self.scanString(),

            else => {
                if (isDigit(c)) {
                    try self.scanInt();
                } else if (std.ascii.isAlphabetic(c) or c == '_') {
                    try self.scanIdentifier();
                } else {
                    return error.UnexpectedCharacter;
                }
            },
        }
    }

    fn scanInt(self: *Lexer) !void {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // TODO handle float in a dedicated type
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        // Here we have the full int!
        try self.addToken(.int, null);
    }

    fn scanString(self: *Lexer) !void {
        // Continue until we reach the end of string
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return error.UnexpectedCharacter;
        }

        // Closing "
        _ = self.advance();

        // Strip surround ", we only want the string value
        const value = self.source[self.start + 1 .. self.current - 1];
        try self.addToken(.string, value);
    }

    fn scanIdentifier(self: *Lexer) !void {
        while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];
        const token_type = TokenType.keyword(text) orelse .identifier;

        try self.addToken(token_type, null);
    }

    // NOTE: -- Helpers

    // Consume and return the current character.
    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    // Append a token using the current lexeme span (or an explicit literal).
    fn addToken(self: *Lexer, token_type: TokenType, literal: ?[]const u8) !void {
        const lexeme = literal orelse self.source[self.start..self.current];
        try self.tokens.append(self.allocator, .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
        });
    }

    // True when all source characters have been consumed.
    fn isAtEnd(self: *const Lexer) bool {
        return self.current >= self.source.len;
    }

    // Return the current character without consuming it.
    fn peek(self: *const Lexer) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.source[self.current];
    }

    // Return the next character without consuming it.
    fn peekNext(self: *const Lexer) u8 {
        if (self.current + 1 >= self.source.len) {
            return 0;
        }
        return self.source[self.current + 1];
    }

    // Consume the current character only if it matches expected.
    fn match(self: *Lexer, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        if (self.source[self.current] != expected) {
            return false;
        }

        self.current += 1;
        return true;
    }
};
