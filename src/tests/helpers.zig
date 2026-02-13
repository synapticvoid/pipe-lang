const std = @import("std");
const pipe = @import("pipe");

pub fn tokenize(source: []const u8, allocator: std.mem.Allocator) ![]const pipe.Token {
    var lexer = pipe.Lexer.init(source, allocator);
    return try lexer.tokenize();
}

pub fn parse(source: []const u8, allocator: std.mem.Allocator) ![]const pipe.ast.Statement {
    const tokens = try tokenize(source, allocator);
    var parser = pipe.Parser.init(tokens, allocator);
    return try parser.parse();
}

pub fn evaluate(source: []const u8, allocator: std.mem.Allocator) !pipe.ast.Value {
    const tokens = try tokenize(source, allocator);
    var parser = pipe.Parser.init(tokens, allocator);
    const statements = try parser.parse();
    var interpreter = pipe.Interpreter.init(allocator);
    defer interpreter.deinit();
    var result: pipe.ast.Value = .null;
    for (statements) |statement| {
        switch (statement) {
            .expression => |expr| result = try interpreter.evaluate(expr),
            .var_declaration => |decl| try interpreter.declare(decl.name, decl.initializer),
        }
    }
    return result;
}