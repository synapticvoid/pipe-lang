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

    // NOTE: -- Public API

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
            return .{ .var_declaration = try self.parseVarDeclarationStatement(ast.Mutability.mutable) };
        }
        if (self.match(&.{.@"const"})) {
            return .{ .var_declaration = try self.parseVarDeclarationStatement(ast.Mutability.constant) };
        }

        if (self.match(&.{.@"fn"})) {
            return .{ .fn_declaration = try self.parseFnDeclarationStatement() };
        }

        if (self.match(&.{.@"error"})) {
            const name = try self.consume(.identifier, "Expect error type name");

            // error Name { V1, V2 }
            if (self.match(&.{.lbrace})) {
                return .{ .error_declaration = try self.parseErrorDeclarationStatement(name) };
            }

            // error Name = A | B
            if (self.match(&.{.equal})) {
                return .{ .error_union_declaration = try self.parseErrorUnionDeclarationStatement(name) };
            }

            return error.UnexpectedToken;
        }

        return try self.parseStatement();
    }

    fn parseVarDeclarationStatement(self: *Parser, mutability: ast.Mutability) ParseError!ast.Statement.VarDeclaration {
        const name = try self.consume(.identifier, "Expect variable name");

        var type_annotation: ?ast.PipeTypeAnnotation = null;
        if (self.match(&.{.colon})) {
            type_annotation = try self.parseReturnType();
        }

        var initializer: ?ast.Expression = null;
        if (self.match(&.{.equal})) {
            initializer = try self.parseExpression();
        }

        _ = try self.consume(.semicolon, "Expect ';' after variable declaration");
        return .{
            .name = name,
            .type_annotation = type_annotation,
            .initializer = initializer,
            .mutability = mutability,
        };
    }

    fn parseFnDeclarationStatement(self: *Parser) ParseError!ast.Statement.FnDeclaration {
        const name = try self.consume(.identifier, "Expect function name.");
        _ = try self.consume(.lparen, "Expect '(' after function name.");

        // Parse parameters
        var params: std.ArrayList(ast.Param) = .{};
        if (!self.check(.rparen)) {
            try params.append(
                self.allocator,
                try self.parseFnParam(),
            );

            while (self.match(&.{.comma})) {
                try params.append(
                    self.allocator,
                    try self.parseFnParam(),
                );
            }
        }
        _ = try self.consume(.rparen, "Expect ')' after parameters.");

        var return_type: ?ast.PipeTypeAnnotation = null;
        if (self.check(.identifier) or self.check(.bang)) {
            return_type = try self.parseReturnType();
        }

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
            .return_type = return_type,
            .body = body.items,
        };
    }

    fn parseFnParam(self: *Parser) ParseError!ast.Param {
        const param_name = try self.consume(.identifier, "Expect parameter name");
        _ = try self.consume(.colon, "Expect ':' after parameter name");
        const param_type = try self.parseReturnType();

        return .{
            .name = param_name,
            .type_annotation = param_type,
        };
    }

    fn parseErrorDeclarationStatement(self: *Parser, name: Token) ParseError!ast.Statement.ErrorDeclaration {
        var variants: std.ArrayList(Token) = .{};
        if (!self.check(.rbrace)) {
            try variants.append(self.allocator, try self.consume(.identifier, "Expect error variant name"));
            while (self.match(&.{.comma})) {
                try variants.append(self.allocator, try self.consume(.identifier, "Expect error variant name"));
            }
        }

        _ = try self.consume(.rbrace, "Expect '}' after error variant list.");

        return .{
            .name = name,
            .variants = variants.items,
        };
    }

    fn parseErrorUnionDeclarationStatement(self: *Parser, name: Token) ParseError!ast.Statement.ErrorUnionDeclaration {
        var members: std.ArrayList(Token) = .{};

        try members.append(self.allocator, try self.consume(.identifier, "Expect error union member name"));
        while (self.match(&.{.pipe})) {
            try members.append(self.allocator, try self.consume(.identifier, "Expect error union member name"));
        }

        return .{
            .name = name,
            .members = members.items,
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

        if (self.check(.@"return")) {
            return .{ .@"return" = try self.parseReturnStatement() };
        }

        return .{ .expression = try self.parseExpressionStatement() };
    }

    fn parseReturnStatement(self: *Parser) ParseError!ast.Statement.Return {
        const token = self.advance();
        var value: ?ast.Expression = null;
        if (!self.check(.semicolon)) {
            value = try self.parseExpression();
        }

        _ = try self.consume(.semicolon, "Expect ';' after return value.");

        return .{ .token = token, .value = value };
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
                    const assignment = try self.allocator.create(ast.Expression.VarAssignment);
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
        if (self.match(&.{.@"try"})) {
            const token = self.previous();
            const expression = try self.parseUnary();

            const try_expr = try self.allocator.create(ast.Expression.Try);
            try_expr.* = .{
                .token = token,
                .expression = expression,
            };

            return .{ .try_expr = try_expr };
        }

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

        while (true) {
            if (self.match(&.{.lparen})) {
                const call = try self.parseCallFn(expr);
                expr = .{ .fn_call = call };
            } else if (self.match(&.{.@"catch"})) {
                const catch_e = try self.parseCallCatch(expr);
                expr = .{ .catch_expr = catch_e };
            } else {
                break;
            }
        }

        return expr;
    }

    fn parseCallFn(self: *Parser, expr: ast.Expression) ParseError!*ast.Expression.FnCall {
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
        return fn_call;
    }

    fn parseCallCatch(self: *Parser, expr: ast.Expression) ParseError!*ast.Expression.Catch {
        const token = self.previous();

        // Parse optional |binding|
        var binding: ?Token = null;
        if (self.match(&.{.pipe})) {
            binding = try self.consume(.identifier, "Expect binding name");
            _ = try self.consume(.pipe, "Expect '|' after binding.");
        }

        // Parse handler expression
        const handler = try self.parseExpression();

        const catch_expr = try self.allocator.create(ast.Expression.Catch);
        catch_expr.* = .{
            .token = token,
            .expression = expr,
            .binding = binding,
            .handler = handler,
        };

        return catch_expr;
    }

    fn parsePrimary(self: *Parser) ParseError!ast.Expression {
        switch (self.peek().type) {
            // Literals
            .int => {
                _ = self.advance();
                const value = try std.fmt.parseFloat(f64, self.previous().lexeme);
                return ast.Expression{ .literal = .{ .value = .{ .int = value } } };
            },
            .string => {
                _ = self.advance();
                return ast.Expression{ .literal = .{ .value = .{ .string = self.previous().lexeme } } };
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
        const condition = try self.parseExpression();

        // Parse then branch
        // TODO: handle optional braces!
        const then_branch = try self.parseExpression();

        // Parse else branch
        var else_branch: ?ast.Expression = null;
        if (self.match(&.{.@"else"})) {
            else_branch = try self.parseExpression();
        }

        const if_expr = try self.allocator.create(ast.Expression.If);
        if_expr.* = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch };
        return if_expr;
    }

    // NOTE: -- Helpers

    // Parse a left-associative binary expression for the given operator set.
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

    fn parseReturnType(self: *Parser) ParseError!ast.PipeTypeAnnotation {
        // Case 1: !T - leading bang means inferred error union
        if (self.match(&.{.bang})) {
            const ok_type = try self.allocator.create(ast.PipeTypeAnnotation);
            ok_type.* = try self.parseReturnType();
            return .{ .inferred_error_union = ok_type };
        }

        // Case 2 and 3: start with an identifier
        const name = try self.consume(.identifier, "Expect type name.");

        // Case 2: E!T - identier was the error set, parse the ok type
        if (self.match(&.{.bang})) {
            const ok_type = try self.allocator.create(ast.PipeTypeAnnotation);
            ok_type.* = try self.parseReturnType();
            return .{ .explicit_error_union = .{
                .error_set = name,
                .ok_type = ok_type,
            } };
        }

        // Case 3: plain named type
        return .{ .named = name };
    }

    // True when the current token is EOF.
    fn isAtEnd(self: *const Parser) bool {
        return self.peek().type == TokenType.eof;
    }

    // Return the current token without consuming it.
    fn peek(self: *const Parser) Token {
        return self.tokens[self.current];
    }

    // Return the most recently consumed token.
    fn previous(self: *const Parser) Token {
        return self.tokens[self.current - 1];
    }

    // Consume and return the current token.
    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    // Advance if the current token matches any of the given types.
    fn match(self: *Parser, types: []const TokenType) bool {
        for (types) |t| {
            if (self.check(t)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    // Expect and consume a specific token, or error.
    fn consume(self: *Parser, token_type: TokenType, error_message: []const u8) ParseError!Token {
        if (self.check(token_type)) {
            return self.advance();
        }
        self.last_error = error_message;
        return error.UnexpectedToken;
    }

    // True if the current token is of the given type (without consuming).
    fn check(self: *const Parser, t: TokenType) bool {
        if (self.isAtEnd()) {
            return false;
        }
        return self.peek().type == t;
    }
};
