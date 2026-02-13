const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Interpreter = @import("interpreter.zig").Interpreter;

const max_file_size = 10 * 1024 * 1024;  // 10MB, should be plenty enough
const max_input_size = 1024 * 1024;  // 1MB, should be plenty enough

fn run(source: []const u8, allocator: std.mem.Allocator) void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    doRun(source, arena.allocator()) catch |err| {
        std.debug.print("Error: {}\n", .{err});
    };
}
fn doRun(source: []const u8, allocator: std.mem.Allocator) !void {
    var lexer = Lexer.init(source, allocator);
    const tokens = try lexer.tokenize();

    var parser = Parser.init(tokens, allocator);
    const statements = try parser.parse();

    var interpreter = try Interpreter.init(allocator);
    try interpreter.interpret(statements);
}

fn runFile(path: []const u8, allocator: std.mem.Allocator) !void {
    const source = try std.fs.cwd().readFileAlloc(allocator, path, max_file_size);
    defer allocator.free(source);
    run(source, allocator);
}

fn repl(allocator: std.mem.Allocator) !void {
    var stdout_buf: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    const stdout: *std.Io.Writer = &stdout_writer.interface;

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

        run(line, allocator);
    }
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    if (args.len > 1) {
        try runFile(args[1], allocator);
    } else {
        try repl(allocator);
    }
}