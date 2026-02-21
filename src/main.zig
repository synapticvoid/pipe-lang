const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const TypeChecker = @import("type_checker.zig").TypeChecker;
const Interpreter = @import("interpreter.zig").Interpreter;
const RuntimeContext = @import("runtime.zig").RuntimeContext;

const max_file_size = 10 * 1024 * 1024; // 10MB, should be plenty enough
const max_input_size = 1024 * 1024; // 1MB, should be plenty enough

fn run(source: []const u8, ctx: RuntimeContext, allocator: std.mem.Allocator) void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    doRun(source, ctx, arena.allocator()) catch |err| switch (err) {
        error.TypeMismatch, error.ConstReassignment, error.UndefinedVariable, error.UndefinedType => {},
        else => std.debug.print("Error: {}\n", .{err}),
    };
}
fn doRun(source: []const u8, ctx: RuntimeContext, allocator: std.mem.Allocator) !void {
    var lexer = Lexer.init(source, allocator);
    const tokens = try lexer.tokenize();

    var parser = Parser.init(tokens, allocator);
    const statements = try parser.parse();

    var type_checker = try TypeChecker.init(allocator);
    type_checker.check(statements) catch |err| {
        if (type_checker.last_error) |msg| {
            std.debug.print("{s}\n", .{msg});
        } else {
            std.debug.print("Type error: {}\n", .{err});
        }
        return err;
    };

    var interpreter = try Interpreter.init(ctx, allocator);
    try interpreter.interpret(statements);
    try ctx.writer.flush();
}

fn runFile(path: []const u8, ctx: RuntimeContext, allocator: std.mem.Allocator) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, path, max_file_size);
    defer allocator.free(source);
    run(source, ctx, allocator);
}

fn repl(ctx: RuntimeContext, allocator: std.mem.Allocator) !void {
    const stdout = ctx.writer;

    var stdin_buf: [max_input_size]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buf);
    const stdin: *std.Io.Reader = &stdin_reader.interface;

    try stdout.writeAll("Pipe REPL\nType exit to quit\n");
    try stdout.flush();

    while (true) {
        try stdout.writeAll(">>> ");
        try stdout.flush();

        // readUntilDelimiter returns ?[]u8 — null means EOF (Ctrl+D)
        const line = try stdin.takeDelimiter('\n') orelse break;

        if (std.mem.eql(u8, line, "exit")) break;

        run(line, ctx, allocator);
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    var stdout_buf: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    const ctx = RuntimeContext{ .writer = &stdout_writer.interface };

    const args = try std.process.argsAlloc(allocator);
    if (args.len > 1) {
        try runFile(args[1], ctx, allocator);
    } else {
        try repl(ctx, allocator);
    }
}
