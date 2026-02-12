const std = @import("std");
const pipe = @import("pipe");

pub fn tokenize(source: []const u8, allocator: std.mem.Allocator) ![]const pipe.Token {
    var lexer = pipe.Lexer.init(source, allocator);
    return try lexer.tokenize();
}

pub fn parse(source: []const u8, allocator: std.mem.Allocator) ![]const pipe.ast.ASTNode {
    const tokens = try tokenize(source, allocator);
    var parser = pipe.Parser.init(tokens, allocator);
    return try parser.parse();
}
