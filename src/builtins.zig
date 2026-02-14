const std = @import("std");
const BuiltinFn = @import("callable.zig").Callable.BuiltinFn;
const Value = @import("ast.zig").Value;
const Environment = @import("environment.zig").Environment;
const RuntimeContext = @import("runtime.zig").RuntimeContext;

pub fn registerAll(env: *Environment) !void {
    try registerFn(env, "print", printFn);
}

fn registerFn(env: *Environment, name: []const u8, function: BuiltinFn.Func ) !void {
    try env.define(name, .{ .function = .{ .builtin = .{
        .name = name,
        .func = function,
    } } });
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
