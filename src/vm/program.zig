const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;

pub const FnObject = struct {
    name: []const u8,
    arity: u8,
    chunk: Chunk,
};

pub const Program = struct {
    chunk: Chunk,
    functions: std.ArrayList(FnObject),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .chunk = Chunk.init(allocator),
            .functions = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program) void {
        self.functions.deinit(self.allocator);
        self.chunk.deinit();
    }

    // Appends a function to the program and returns its index
    pub fn addFunction(self: *Program, fn_object: FnObject) !u16 {
        try self.functions.append(self.allocator, fn_object);
        return @intCast(self.functions.items.len - 1);
    }
};
