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

pub const EvalResult = struct {
    value: pipe.ast.Value,
    output: []const u8,
    allocating: std.Io.Writer.Allocating,

    pub fn deinit(self: *EvalResult) void {
        self.allocating.deinit();
    }
};

pub fn evaluate(source: []const u8, allocator: std.mem.Allocator) !EvalResult {
    const tokens = try tokenize(source, allocator);
    var parser = pipe.Parser.init(tokens, allocator);
    const statements = try parser.parse();

    var aw: std.Io.Writer.Allocating = .init(allocator);
    const ctx = pipe.RuntimeContext{ .writer = &aw.writer };
    var interpreter = try pipe.Interpreter.init(ctx, allocator);
    defer interpreter.deinit();

    var result: pipe.ast.Value = .null;
    for (statements) |statement| {
        switch (statement) {
            .expression => |expr| result = try interpreter.evaluate(expr),
            .var_declaration => |decl| {
                try interpreter.declareVar(decl);
                result = .unit;
            },
            .fn_declaration => |decl| {
                try interpreter.declareFn(decl);
                result = .null;
            },
        }
    }
    return .{
        .value = result,
        .output = aw.written(),
        .allocating = aw,
    };
}
