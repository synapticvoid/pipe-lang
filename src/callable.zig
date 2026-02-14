const ast = @import("ast.zig");
const Environment = @import("environment.zig").Environment;

pub const PipeFunction = struct {
    declaration: ast.Statement.FnDeclaration,
    closure: *Environment,
};
