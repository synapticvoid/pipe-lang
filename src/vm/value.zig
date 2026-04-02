const std = @import("std");
const activeTag = std.meta.activeTag;
const RuntimeContext = @import("../runtime.zig").RuntimeContext;
const StructKind = @import("../ast.zig").StructKind;

pub const VmError = error{
    ArityMismatch,
    DivisionByZero,
    NotCallable, // Not a function/method that we can call
    OutOfMemory,
    TypeError, // wrong types for arithmetic/negate/table
    UndefinedField,
    UndefinedVariable,
};

pub const Value = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    null,
    unit,

    // Index of the VM function
    // Stored in the function table in Program
    function: u16,

    // Builtin function
    native: NativeFn,

    // pointer so that copies of a value share identity (u2 = u1 means u1.field == u2.field)
    // var u1 = User("Bob");
    // var u2 = u1;
    // if (u1.name == u2.name) { ... } // true
    struct_instance: *StructInstance,

    // Index of a struct constructor
    // Stored in a struct defs table in Program
    //
    // This makes struct constructor first-class citizens:
    // enum Color { Red };
    // transform(Color.Red); // Constructor is passed as a variable
    struct_constructor: u16,

    bound_method: BoundMethod,

    pub const StructInstance = struct {
        type_name: []const u8,
        field_names: []const []const u8,
        field_values: []Value,
        body_field_values: []Value,
        body_field_names: []const []const u8,
        kind: StructKind,
    };

    pub const BoundMethod = struct {
        fn_idx: u16,
        receiver: *StructInstance,
    };

    pub fn eql(self: Value, other: Value) bool {
        const self_tag = activeTag(self);
        const other_tag = activeTag(other);
        if (self_tag != other_tag) {
            return false;
        }

        return switch (self) {
            .int => |a| a == other.int,
            .string => |a| std.mem.eql(u8, a, other.string),
            .boolean => |a| a == other.boolean,
            .null, .unit => true,
            .function => |a| a == other.function,
            .native => |a| a.func == other.native.func,
            .struct_instance => |a| {
                const b = other.struct_instance;
                if (a.kind == .plain) {
                    return a == b;
                }

                if (a.kind != b.kind) {
                    return false;
                }

                if (!std.mem.eql(u8, a.type_name, b.type_name)) {
                    return false;
                }

                if (a.field_values.len != b.field_values.len) {
                    return false;
                }

                for (a.field_values, b.field_values) |av, bv| {
                    if (!av.eql(bv)) {
                        return false;
                    }
                }
                return true;
            },
            .struct_constructor => |a| a == other.struct_constructor,
            .bound_method => |a| {
                const b = other.bound_method;
                return a.fn_idx == b.fn_idx and a.receiver == b.receiver;
            },
        };
    }

    pub fn asInt(self: Value) !i64 {
        switch (self) {
            .int => |n| return n,
            else => return error.TypeError,
        }
    }

    // Returns true if the value is truthy
    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .null, .unit => false,
            .boolean => |b| b,
            .int => |n| n != 0,
            .string => |s| s.len > 0,
            .function,
            .native,
            .struct_instance,
            .struct_constructor,
            .bound_method,
            => true,
        };
    }

    pub fn isNumber(self: Value) bool {
        return switch (self) {
            .int => true,
            else => false,
        };
    }

    pub fn format(self: Value, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            .int => |n| try writer.print("{d}", .{n}),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .function => try writer.writeAll("fn"),
            .native => |b| try writer.print("<builtin {s}>", .{b.name}),
            .null => try writer.writeAll("null"),
            .unit => try writer.writeAll("unit"),
            .struct_instance => |si_ptr| {
                const si = si_ptr.*;
                switch (si.kind) {
                    .plain => try writer.print("<{s}>", .{si.type_name}),
                    .case => {
                        try writer.print("{s}(", .{si.type_name});
                        for (si.field_names, si.field_values, 0..) |name, value, i| {
                            if (i > 0) {
                                try writer.writeAll(", ");
                            }
                            try writer.print("{s}=", .{name});
                            try value.format(writer);
                        }
                        try writer.writeAll(")");
                    },
                }
            },
            .struct_constructor => |sc| try writer.print("constructor<{d}>", .{sc}),
            .bound_method => |bm| try writer.print("method<{d}>", .{bm.fn_idx}),
        }
    }
};

pub const NativeFn = struct {
    pub const Func = *const fn (args: []const Value, ctx: RuntimeContext, caller: Caller) VmError!Value;

    name: []const u8,
    arity: ?u8, // null = variadic
    func: Func,
};

// Interface to abstract the VM from a native function call
// The goal is the let a native function call a VM function without leaking the VM state.
pub const Caller = struct {
    // VM pointer
    ptr: *anyopaque,

    // Function pointers
    // Call a VM function
    call_ptr: *const fn (ptr: *anyopaque, callable: Value, args: []const Value) VmError!Value,

    // Resolve a VM function by name
    resolve_method_ptr: *const fn (ptr: *anyopaque, instance: *Value.StructInstance, name: []const u8) VmError!?Value,

    pub fn call(self: Caller, callable: Value, args: []const Value) VmError!Value {
        return self.call_ptr(self.ptr, callable, args);
    }

    pub fn resolveMethod(self: Caller, instance: *Value.StructInstance, name: []const u8) VmError!?Value {
        return self.resolve_method_ptr(self.ptr, instance, name);
    }
};
