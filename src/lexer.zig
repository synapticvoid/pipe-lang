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

    fn scanToken(self: *Lexer) !void {
        const c = self.advance();

        switch (c) {
            '+' => try self.addToken(.plus, null),
            '-' => try self.addToken(.minus, null),
            '*' => try self.addToken(.star, null),
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
            '(' => try self.addToken(.lparen, null),
            ')' => try self.addToken(.rparen, null),
            '{' => try self.addToken(.lbrace, null),
            '}' => try self.addToken(.rbrace, null),
            ';' => try self.addToken(.semicolon, null),
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
            '\n' => self.line += 1,
            // Ignore whitespaces
            ' ', '\r', '\t' => {},
            else => {
                if (isDigit(c)) {
                    try self.number();
                } else {
                    return error.UnexpectedCharacter;
                }
            },
        }
    }

    fn advance(self: *Lexer) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    fn addToken(self: *Lexer, token_type: TokenType, literal: ?[]const u8) !void {
        const lexeme = literal orelse self.source[self.start..self.current];
        try self.tokens.append(self.allocator, .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
        });
    }

    fn isAtEnd(self: *const Lexer) bool {
        return self.current >= self.source.len;
    }

    fn peek(self: *const Lexer) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.source[self.current];
    }

    fn peekNext(self: *const Lexer) u8 {
        if (self.current + 1 >= self.source.len) {
            return 0;
        }
        return self.source[self.current + 1];
    }

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

    fn number(self: *Lexer) !void {
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Is it a float?
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        // Here we have the full number!
        try self.addToken(.number, null);
    }
};
