const ast = @import("ast.zig");
const Environment = @import("environment.zig").Environment;
const RuntimeContext = @import("runtime.zig").RuntimeContext;

pub const Callable = union(enum) {
    user: UserFn,
    builtin: BuiltinFn,
    struct_constructor: StructConstructor,

    pub const UserFn = struct {
        declaration: ast.Statement.FnDeclaration,
        // Synthezised result enum name (e.g. MathError!Int) if fallible, null otherwise.
        // Used at call time to wrap the return value in Ok or Err
        result_name: ?[]const u8,
        closure: *Environment,
    };

    pub const BuiltinFn = struct {
        pub const Func = *const fn (args: []const ast.Value, ctx: RuntimeContext) ast.Value;

        name: []const u8,
        func: Func,
    };

    pub const StructConstructor = struct {
        name: []const u8,
        field_names: []const []const u8,
        body_field_names: []const []const u8,
        body_field_defaults: []const ast.Expression,
        kind: ast.StructKind,
    };
};
