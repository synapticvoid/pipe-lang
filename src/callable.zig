const ast = @import("ast.zig");
const Environment = @import("environment.zig").Environment;

pub const Callable = union(enum) {
    user: UserFn,
    builtin: BuiltinFn,

    pub const UserFn = struct {
        declaration: ast.Statement.FnDeclaration,
        closure: *Environment,
    };

    pub const BuiltinFn = struct {
        name: []const u8,
        func: *const fn(args: []const ast.Value) ast.Value,
    };
};

