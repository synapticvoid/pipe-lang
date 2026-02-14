const std = @import("std");
const tok = @import("tokens.zig");
const ast = @import("ast.zig");
const Token = tok.Token;
const TokenType = tok.TokenType;

const ParseError = error{
    UnexpectedToken,
    InvalidAssignmentTarget,
    OutOfMemory,
    InvalidCharacter,
};

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

    pub fn parse(self: *Parser) ParseError![]ast.Statement {
        var statements: std.ArrayList(ast.Statement) = .{};

        while (!self.isAtEnd()) {
            try statements.append(self.allocator, try self.parseDeclaration());
        }
        return statements.items;
    }

    // NOTE: -- Declarations

    fn parseDeclaration(self: *Parser) ParseError!ast.Statement {
        if (self.match(&.{.@"var"})) {
            return .{ .var_declaration = try self.parseVarDeclaration() };
        }

        if (self.match(&.{.@"fn"})) {
            return .{ .fn_declaration = try self.parseFnDeclaration() };
        }

        return try self.parseStatement();
    }

    fn parseVarDeclaration(self: *Parser) ParseError!ast.Statement.VarDeclaration {
        const name = try self.consume(.identifier, "Expect variable name");

        var initializer: ?ast.Expression = null;
        if (self.match(&.{.equal})) {
            initializer = try self.parseExpression();
        }

        _ = try self.consume(.semicolon, "Expect ';' after variable declaration");
        return .{
            .name = name,
            .initializer = initializer,
        };
    }

    fn parseFnDeclaration(self: *Parser) ParseError!ast.Statement.FnDeclaration {
        const name = try self.consume(.identifier, "Expect function name.");
        _ = try self.consume(.lparen, "Expect '(' after function name.");

        // Parse parameters
        var params: std.ArrayList(Token) = .{};
        if (!self.check(.rparen)) {
            try params.append(self.allocator, try self.consume(.identifier, "Expect parameter name"));
            while (self.match(&.{.comma})) {
                try params.append(self.allocator, try self.consume(.identifier, "Expect parameter name"));
            }
        }
        _ = try self.consume(.rparen, "Expect ')' after parameters.");
        _ = try self.consume(.lbrace, "Expect '{' before function body.");

        // Parse body
        var body: std.ArrayList(ast.Statement) = .{};
        while (!self.check(.rbrace)) {
            try body.append(self.allocator, try self.parseDeclaration());
        }
        _ = try self.consume(.rbrace, "Expect '}' after function body.");

        return .{
            .name = name,
            .params = params.items,
            .body = body.items,
        };
    }

    // NOTE: -- Statements

    fn parseStatement(self: *Parser) ParseError!ast.Statement {
        if (self.check(.lbrace)) {
            return .{ .expression = .{ .block = try self.parseBlock() } };
        }

        if (self.check(.@"if")) {
            return .{ .expression = .{ .if_expr = try self.parseIf() } };
        }

        return .{ .expression = try self.parseExpressionStatement() };
    }

    fn parseExpressionStatement(self: *Parser) ParseError!ast.Expression {
        const expr = try self.parseExpression();
        _ = try self.consume(TokenType.semicolon, "Expect ; after expression");
        return expr;
    }

    // NOTE: -- Expressions

    fn parseExpression(self: *Parser) ParseError!ast.Expression {
        return self.parseAssignment();
    }

    fn parseAssignment(self: *Parser) ParseError!ast.Expression {
        const expr = try self.parseEquality();

        if (self.match(&.{.equal})) {
            const equals = self.previous();
            const value = try self.parseAssignment();

            switch (expr) {
                .variable => {
                    const assignment = try self.allocator.create(ast.Expression.VariableAssignment);
                    assignment.* = .{ .token = expr.variable.token, .value = value };
                    return ast.Expression{ .var_assignment = assignment };
                },
                else => {
                    self.last_error = std.fmt.allocPrint(self.allocator, "Invalid assignment target. token: {s}", .{equals.lexeme}) catch null;
                    return error.InvalidAssignmentTarget;
                },
            }
        }

        return expr;
    }

    fn parseEquality(self: *Parser) ParseError!ast.Expression {
        return self.parseBinaryLeft(parseComparison, &.{ .equal_equal, .bang_equal });
    }

    fn parseComparison(self: *Parser) ParseError!ast.Expression {
        return self.parseBinaryLeft(parseTerm, &.{ .greater, .greater_equal, .less, .less_equal });
    }

    fn parseTerm(self: *Parser) ParseError!ast.Expression {
        return self.parseBinaryLeft(parseFactor, &.{ .plus, .minus });
    }

    fn parseFactor(self: *Parser) ParseError!ast.Expression {
        return self.parseBinaryLeft(parseUnary, &.{ .star, .slash });
    }

    fn parseUnary(self: *Parser) ParseError!ast.Expression {
        if (self.match(&.{ TokenType.bang, TokenType.minus })) {
            const operator = self.previous();
            const right = try self.parseUnary();
            const unary = try self.allocator.create(ast.Expression.Unary);
            unary.* = .{
                .operator = operator,
                .right = right,
            };

            return .{ .unary = unary };
        }
        return self.parseCall();
    }

    fn parseCall(self: *Parser) ParseError!ast.Expression {
        var expr = try self.parsePrimary();

        while (self.match(&.{.lparen})) {
            var args: std.ArrayList(ast.Expression) = .{};
            if (!self.check(.rparen)) {
                try args.append(self.allocator, try self.parseExpression());
                while (self.match(&.{.comma})) {
                    try args.append(self.allocator, try self.parseExpression());
                }
            }
            _ = try self.consume(.rparen, "Expect ')' after arguments.");

            const fn_call = try self.allocator.create(ast.Expression.FnCall);
            fn_call.* = .{ .callee = expr, .args = args.items };
            expr = .{ .fn_call = fn_call };
        }

        return expr;
    }

    fn parsePrimary(self: *Parser) ParseError!ast.Expression {
        switch (self.peek().type) {
            // Literals
            .int => {
                _ = self.advance();
                const value = try std.fmt.parseFloat(f64, self.previous().lexeme);
                return ast.Expression{ .literal = .{ .value = .{ .int = value } } };
            },
            .true => {
                _ = self.advance();
                return ast.Expression{ .literal = .{ .value = .{ .boolean = true } } };
            },
            .false => {
                _ = self.advance();
                return ast.Expression{ .literal = .{ .value = .{ .boolean = false } } };
            },
            .null => {
                _ = self.advance();
                return ast.Expression{ .literal = .{ .value = .null } };
            },

            // Variables
            .identifier => {
                _ = self.advance();
                return ast.Expression{ .variable = .{ .token = self.previous() } };
            },

            // Grouping
            .lparen => {
                _ = self.advance();
                const expr = try self.parseExpression();
                _ = try self.consume(.rparen, "Expect ')' after expression.");
                return expr;
            },

            // Control flow
            .lbrace => {
                return .{ .block = try self.parseBlock() };
            },

            .@"if" => {
                return .{ .if_expr = try self.parseIf() };
            },

            else => return error.UnexpectedToken,
        }
    }

    fn parseBlock(self: *Parser) ParseError!*ast.Expression.Block {
        _ = try self.consume(.lbrace, "Expect '{' at beginning of block.");
        var statements: std.ArrayList(ast.Statement) = .{};

        while (!self.check(.rbrace)) {
            try statements.append(self.allocator, try self.parseDeclaration());
        }
        _ = try self.consume(.rbrace, "Expect '}' at end of block.");

        const block = try self.allocator.create(ast.Expression.Block);
        block.* = .{ .statements = statements.items };
        return block;
    }

    fn parseIf(self: *Parser) ParseError!*ast.Expression.If {
        _ = try self.consume(.@"if", "Expect 'if'.");
        // Parse condition
        _ = try self.consume(.lparen, "Expect '(' after 'if'.");
        const condition = try self.parseExpression();
        _ = try self.consume(.rparen, "Expect ')' after if condition.");

        // Parse then branch
        // TODO handle optional braces!
        const then_branch = try self.parseExpression();
        var else_branch: ?ast.Expression = null;
        if (self.match(&.{.@"else"})) {
            else_branch = try self.parseExpression();
        }

        const if_expr = try self.allocator.create(ast.Expression.If);
        if_expr.* = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch };
        return if_expr;
    }

    // NOTE: -- Utils

    fn parseBinaryLeft(self: *Parser, parse_operand: *const fn (*Parser) ParseError!ast.Expression, operators: []const TokenType) ParseError!ast.Expression {
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

    fn consume(self: *Parser, token_type: TokenType, error_message: []const u8) ParseError!Token {
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
