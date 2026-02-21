const std = @import("std");
const BuiltinFn = @import("callable.zig").Callable.BuiltinFn;
const Environment = @import("environment.zig").Environment;
const PipeType = @import("types.zig").PipeType;
const RuntimeContext = @import("runtime.zig").RuntimeContext;
const TypeEnvironment = @import("type_checker.zig").TypeEnvironment;
const TypeInfo = @import("type_checker.zig").TypeInfo;
const Value = @import("ast.zig").Value;

pub fn registerAll(env: *Environment) !void {
    try registerFn(env, "print", printFn);
    try registerFn(env, "fail", failFn);
}

pub fn registerAllTypes(env: *TypeEnvironment) !void {
    try env.define("print", .{ .function = .{
        .param_types = &.{PipeType.any},
        .return_type = PipeType.unit,
    } });

    try env.define("fn", .{ .function = .{
        .param_types = &.{PipeType.string},
        .return_type = PipeType{ .error_set = "Error" },
    } });
}

fn registerFn(env: *Environment, name: []const u8, function: BuiltinFn.Func) !void {
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

fn failFn(args: []const Value, _: RuntimeContext) Value {
    return .{ .error_value = .{ .message = args[0].string } };
}
