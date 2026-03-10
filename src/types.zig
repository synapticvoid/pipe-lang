const std = @import("std");
const activeTag = std.meta.activeTag;

const ast = @import("ast.zig");

pub const SELF_TYPE = "Self";
pub const RESULT_OK_VARIANT = "Ok";
pub const RESULT_ERR_VARIANT = "Err";
pub const METHOD_EQUALS = "equals";
pub const METHOD_TO_STRING = "to_str";

pub const PipeType = union(enum) {
    int,
    float,
    bool,
    string,
    unit,
    any,

    // Structs

    struct_type: []const u8,
    enum_type: []const u8,

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
            if (self_tag == .struct_type) {
                return std.mem.eql(u8, self.struct_type, other.struct_type);
            }

            if (self_tag == .enum_type) {
                return std.mem.eql(u8, self.enum_type, other.enum_type);
            }

            return true;
        }

        if (self_tag == .any or other_tag == .any) {
            return true;
        }

        return false;
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

pub const TypeDef = union(enum) {
    struct_type: StructTypeInfo,
    enum_type: EnumTypeInfo,
};

pub const StructTypeInfo = struct {
    fields: []const FieldInfo,
    body_fields: []const FieldInfo,
    kind: ast.StructKind,
};

pub const EnumTypeInfo = struct {
    // true for a user-declared error enum, a type that can appear in E!T position
    is_error: bool,

    // internal only - marks the synthesized Ok/Err enum.
    // It's an internal wrapper the type checker creates for E!T
    is_result: bool,
    variants: []const VariantTypeInfo,
};

pub const VariantTypeInfo = struct {
    name: []const u8,
    fields: []const FieldInfo,
};

pub const FieldInfo = struct {
    name: []const u8,
    pipe_type: PipeType,
    mutability: ast.Mutability,
};
