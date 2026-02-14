const std = @import("std");

pub const RuntimeContext = struct {
    writer: *std.Io.Writer,
};
