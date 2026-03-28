const std = @import("std");

const RuntimeContext = @import("../runtime.zig").RuntimeContext;
const NativeFn = @import("value.zig").NativeFn;
const Value = @import("value.zig").Value;

pub fn registerAll(globals: *std.StringHashMapUnmanaged(Value), allocator: std.mem.Allocator) !void {
    try registerFn(globals, "print", printFn, allocator);
}

fn registerFn(
    globals: *std.StringHashMapUnmanaged(Value),
    name: []const u8,
    func: NativeFn.Func,
    allocator: std.mem.Allocator,
) !void {
    try globals.put(allocator, name, .{ .native = .{
        .name = name,
        .arity = null,
        .func = func,
    } });
}

fn printFn(args: []const Value, ctx: RuntimeContext) Value {
    for (args, 0..) |arg, i| {
        if (i > 0) ctx.writer.writeByte(' ') catch {};
        switch (arg) {
            .string => |s| ctx.writer.writeAll(s) catch {},
            else => arg.format(ctx.writer) catch {},
        }
    }
    ctx.writer.writeByte('\n') catch {};
    return .unit;
}
