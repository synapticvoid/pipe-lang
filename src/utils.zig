const std = @import("std");

/// Returns "type_name.member_name" as a heap-allocated string.
pub fn qualifiedName(allocator: std.mem.Allocator, type_name: []const u8, member_name: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}.{s}", .{ type_name, member_name });
}

// Return "err_type_name!ok_type_name" as a heap-allocated string.
pub fn resultQualifiedName(allocator: std.mem.Allocator, err_type_name: []const u8, ok_type_name: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}!{s}", .{ err_type_name, ok_type_name });
}
