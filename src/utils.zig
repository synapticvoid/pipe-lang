const std = @import("std");

/// Returns "type_name.member_name" as a heap-allocated string.
pub fn memberName(allocator: std.mem.Allocator, type_name: []const u8, member_name: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}.{s}", .{ type_name, member_name });
}

// Return "err_type_name!ok_type_name" as a heap-allocated string.
pub fn resultTypeName(allocator: std.mem.Allocator, err_type_name: []const u8, ok_type_name: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}!{s}", .{ err_type_name, ok_type_name });
}

// Check if variant_name is a variant of type_name
pub fn isVariant(type_name: []const u8, variant_name: []const u8) bool {
    const has_dot_prefix = type_name.len > variant_name.len and type_name[type_name.len - variant_name.len - 1] == '.';
    // Length comparison to speed-up
    // if type_name contains a '.', it's an enum type
    // if type_name ends with variant_name, it's a variant
    return has_dot_prefix and std.mem.endsWith(u8, type_name, variant_name);
}
