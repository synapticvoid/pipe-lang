const std = @import("std");
const Lexer = @import("pipe").Lexer;
const expect = std.testing.expect;

test "tokenize simple expression" {
    try expect(true);
}