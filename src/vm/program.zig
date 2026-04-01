const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const StructKind = @import("../ast.zig").StructKind;

// VM function object emitted by the compiler.
//
// Ownership/lifetime note:
// - `chunk` may contain heap-backed buffers.
// - In the current pipeline we allocate Program-related data in an Arena,
//   so bulk arena teardown handles nested memory.
// - `Program.deinit()` is therefore intentionally shallow for function items.
pub const FnObject = struct {
    name: []const u8,
    arity: u8,
    chunk: Chunk,
    // Synthezised result enum name (e.g. MathError!Int) if fallible, null otherwise.
    // Used at call time to wrap the return value in Ok or Err
    result_name: ?[]const u8,
};

// Metadata used by `OpCode.construct` to materialize a runtime struct instance.
//
// Ownership/lifetime note:
// - This definition currently stores slices and names with arena lifetime.
// - `Program.deinit()` only deinitializes the ArrayList container; it does not
//   deep-free nested slices/strings.
pub const StructDef = struct {
    name: []const u8,
    field_names: []const []const u8,
    body_field_names: []const []const u8,
    kind: StructKind,
    body_default_fn: ?u16,
};

pub const Program = struct {
    chunk: Chunk,
    functions: std.ArrayList(FnObject),
    struct_defs: std.ArrayList(StructDef),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .chunk = Chunk.init(allocator),
            .functions = .{},
            .struct_defs = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Program) void {
        // Shallow deinit by design: Program is currently arena-backed in normal
        // execution, so nested allocations are reclaimed when the arena deinit's.
        self.functions.deinit(self.allocator);
        self.struct_defs.deinit(self.allocator);
        self.chunk.deinit();
    }

    // Appends a function to the program and returns its index
    pub fn addFunction(self: *Program, fn_object: FnObject) !u16 {
        try self.functions.append(self.allocator, fn_object);
        return @intCast(self.functions.items.len - 1);
    }

    // Appends a struct definition to the program and returns its index
    pub fn addStructDef(self: *Program, struct_def: StructDef) !u16 {
        try self.struct_defs.append(self.allocator, struct_def);
        return @intCast(self.struct_defs.items.len - 1);
    }
};
