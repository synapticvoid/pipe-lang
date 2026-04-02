const std = @import("std");

const types = @import("../types.zig");
const RuntimeContext = @import("../runtime.zig").RuntimeContext;
const Caller = @import("value.zig").Caller;
const NativeFn = @import("value.zig").NativeFn;
const Value = @import("value.zig").Value;
const VmError = @import("value.zig").VmError;

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

fn printFn(args: []const Value, ctx: RuntimeContext, caller: Caller) VmError!Value {
    for (args, 0..) |arg, i| {
        if (i > 0) ctx.writer.writeByte(' ') catch {};
        switch (arg) {
            .string => |s| ctx.writer.writeAll(s) catch {},
            .struct_instance => |si| {
                const method = try caller.resolveMethod(si, types.METHOD_TO_STRING);
                if (method) |m| {
                    const ret_value = try caller.call(m, &.{});
                    if (ret_value != .string) {
                        return VmError.TypeError;
                    }

                    ctx.writer.writeAll(ret_value.string) catch {};
                } else {
                    arg.format(ctx.writer) catch {};
                }
            },
            else => arg.format(ctx.writer) catch {},
        }
    }
    ctx.writer.writeByte('\n') catch {};
    return .unit;
}
