const std = @import("std");
const activeTag = std.meta.activeTag;

pub const PipeType = union(enum) {
    int,
    float,
    bool,
    string,
    unit,
    any,

    // Errors

    // error MyError { V1, V2 }
    error_set: []const u8,

    // E!T or !T
    error_union: struct {
        // null == inferred (!T)
        error_set: ?[]const u8,
        ok_type: *PipeType,
    },

    pub fn isNumeric(self: PipeType) bool {
        switch (self) {
            .int, .float => return true,
            else => return false,
        }
    }

    pub fn compatible(self: PipeType, other: PipeType) bool {
        const self_tag = activeTag(self);
        const other_tag = activeTag(other);
        if (self_tag == other_tag) {
            return true;
        }

        if (self_tag == .any or other_tag == .any) {
            return true;
        }

        if (self_tag == .error_set and other_tag == .error_set) {
            return true;
        }

        // T is compatible with E!T (returing an ok value from a fallible function)
        if (other_tag == .error_union) {
            return self.compatible(other.error_union.ok_type.*);
        }

        return false;
    }

    // Returns true of this type carries a possible error
    pub fn isFallible(self: PipeType) bool {
        return activeTag(self) == .error_union;
    }

    // Return true if self's error is a subset of other's (for try/catch checking)
    pub fn isSubError(self: PipeType, other: PipeType) bool {
        // TODO: Implement in TypeChecker (which holds the registry) and wire into
        // checkTry/checkCatch to verify error set compatibility at try sites.
        _ = self;
        _ = other;
        return false;
    }
};
