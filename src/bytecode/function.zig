const Chunk = @import("chunk.zig").Chunk;

pub const FnObject = struct {
    name: []const u8,
    arity: u8,
    chunk: Chunk,
};
