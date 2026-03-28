const std = @import("std");
const FnObject = @import("function.zig").FnObject;
const Chunk = @import("chunk.zig").Chunk;

pub const Module = struct {
    chunk: Chunk,
    functions: std.ArrayList(FnObject),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Module {
        return .{
            .chunk = Chunk.init(allocator),
            .functions = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Module) void {
        self.functions.deinit(self.allocator);
        self.chunk.deinit();
    }

    // Appends a function to the module and returns its index
    pub fn addFunction(self: *Module, fn_object: FnObject) !u16 {
        try self.functions.append(self.allocator, fn_object);
        return @intCast(self.functions.items.len - 1);
    }
};
