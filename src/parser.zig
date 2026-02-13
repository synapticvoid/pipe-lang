const std = @import("std");
const tok = @import("tokens.zig");
const ast = @import("ast.zig");
const Token = tok.Token;
const TokenType = tok.TokenType;

pub const Parser = struct {
    tokens: []const Token,
    current: usize,
    allocator: std.mem.Allocator,
    last_error: ?[]const u8 = null,

    pub fn init(tokens: []const Token, allocator: std.mem.Allocator) Parser {
        return .{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Parser) ![]ast.Statement {
        var statements: std.ArrayList(ast.Statement) = .{};

        while (!self.isAtEnd()) {
            try statements.append(self.allocator, try self.parseDeclaration());
        }
        return statements.items;
    }

    // NOTE: -- Declarations

    fn parseDeclaration(self: *Parser) !ast.Statement {
        return try self.parseStatement();
    }

    // NOTE: -- Statements

    fn parseStatement(self: *Parser) !ast.Statement {
        return try self.parseExpressionStatement();
    }

    fn parseExpressionStatement(self: *Parser) !ast.Statement {
        const expr = try self.parseExpression();
        _ = try self.consume(TokenType.semicolon, "Expect ; after expression");
        return ast.Statement{ .expression = expr };
    }

    // NOTE: -- Expressions

    fn parseExpression(self: *Parser) !ast.Expression {
        return self.parseEquality();
    }

    fn parseEquality(self: *Parser) !ast.Expression {
        return self.parseBinaryLeft(parseComparison, &.{ .equal_equal, .bang_equal });
    }

    fn parseComparison(self: *Parser) !ast.Expression {
        return self.parseBinaryLeft(parseTerm, &.{ .greater, .greater_equal, .less, .less_equal });
    }

    fn parseTerm(self: *Parser) !ast.Expression {
        return self.parseBinaryLeft(parseFactor, &.{ .plus, .minus });
    }

    fn parseFactor(self: *Parser) !ast.Expression {
        return self.parseBinaryLeft(parsePrimary, &.{ .star, .slash });
    }

    fn parsePrimary(self: *Parser) !ast.Expression {
        if (self.match(&.{TokenType.number})) {
            const value = try std.fmt.parseFloat(f64, self.previous().lexeme);
            return ast.Expression{
                .literal = .{ .value = .{ .number = value } },
            };
        }

        if (self.match(&.{TokenType.lparen})) {
            const expr = try self.parseExpression();
            _ = try self.consume(TokenType.rparen, "Expect ')' after expression.");
            return expr;
        }

        return error.UnexpectedToken;
    }

    // NOTE: -- Utils

    fn parseBinaryLeft(self: *Parser, parse_operand: *const fn (*Parser) anyerror!ast.Expression, operators: []const TokenType) !ast.Expression {
        var left = try parse_operand(self);

        while (self.match(operators)) {
            const operator = self.previous();
            const right = try parse_operand(self);
            const binary = try self.allocator.create(ast.Expression.Binary);
            // .* to dereference the pointer (*Binary allocated on the heap)
            binary.* = .{ .left = left, .operator = operator, .right = right };
            left = .{ .binary = binary };
        }

        return left;
    }

    fn isAtEnd(self: *const Parser) bool {
        return self.peek().type == TokenType.eof;
    }

    fn peek(self: *const Parser) Token {
        return self.tokens[self.current];
    }

    fn previous(self: *const Parser) Token {
        return self.tokens[self.current - 1];
    }

    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Parser, token_type: TokenType, error_message: []const u8) !Token {
        if (self.check(token_type)) {
            return self.advance();
        }
        self.last_error = error_message;
        return error.UnexpectedToken;
    }

    fn check(self: *const Parser, t: TokenType) bool {
        if (self.isAtEnd()) {
            return false;
        }
        return self.peek().type == t;
    }
};
