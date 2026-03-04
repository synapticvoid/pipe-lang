const std = @import("std");
const helpers = @import("helpers");

fn expectTypeCheck(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    try helpers.typeCheck(source, arena.allocator());
}

fn expectTypeError(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck(source, arena.allocator());
    try std.testing.expectError(error.TypeMismatch, result);
}

fn expectUnconsumedFallibleError(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck(source, arena.allocator());
    try std.testing.expectError(error.UnconsumedFallible, result);
}

fn expectConstError(source: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck(source, arena.allocator());
    try std.testing.expectError(error.ConstReassignment, result);
}

test "return type matches declaration" {
    try expectTypeCheck("fn five() Int { return 5; }");
    try expectTypeCheck("fn greet() { return; }");
    try expectTypeCheck("fn greet() Unit { return; }");
    try expectTypeCheck("fn add(a: Int, b: Int) Int { return a + b; }");
}

test "return type mismatch" {
    try expectTypeError("fn five() Int { return true; }");
    try expectTypeError("fn flag() Bool { return 42; }");
}

test "implicit return type checked" {
    try expectTypeCheck("fn five() Int { 5; }");
    try expectTypeError("fn five() Int { true; }");
}

test "const reassignment rejected" {
    try expectConstError("const x = 5; x = 10;");
}

test "var reassignment allowed" {
    try expectTypeCheck("var x = 5; x = 10;");
}

test "var type mismatch on reassignment" {
    try expectTypeError("var x = 5; x = true;");
}

// -- Error type declarations

test "error enum declaration is valid" {
    try expectTypeCheck("error enum MathError { DivByZero, Overflow }");
}

test "error enum composition is valid" {
    try expectTypeCheck(
        \\error enum MathError { DivByZero }
        \\error enum AppError { MathError }
    );
}

test "explicit error union with unknown error set is rejected" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck("fn f() Unknown!Int { 1; }", arena.allocator());
    try std.testing.expectError(error.UndefinedType, result);
}

// -- Fallible functions

test "fallible function with ok return is valid" {
    try expectTypeCheck(
        \\error enum MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
    );
}

// -- try

test "try inside fallible function is valid" {
    try expectTypeCheck(
        \\error enum MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) MathError!Int { try divide(a, b); }
    );
}

test "try outside fallible function is rejected" {
    try expectTypeError(
        \\error enum MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) Int { try divide(a, b); }
    );
}

test "try on non-fallible expression is rejected" {
    try expectTypeError(
        \\error enum DummyError { Fail }
        \\fn add(a: Int, b: Int) Int { a + b; }
        \\fn safe(a: Int, b: Int) DummyError!Int { try add(a, b); }
    );
}

// -- catch

test "catch on fallible expression is valid" {
    try expectTypeCheck(
        \\error enum MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) Int { divide(a, b) catch { -1; }; }
    );
}

test "catch with binding is valid" {
    try expectTypeCheck(
        \\error enum MathError { DivByZero }
        \\fn divide(a: Int, b: Int) MathError!Int { a / b; }
        \\fn safe(a: Int, b: Int) Int { divide(a, b) catch |e| { -1; }; }
    );
}

test "catch on non-fallible expression is rejected" {
    try expectTypeError(
        \\fn add(a: Int, b: Int) Int { a + b; }
        \\fn safe(a: Int, b: Int) Int { add(a, b) catch { 0; }; }
    );
}

// -- Structs

test "struct declaration is valid" {
    try expectTypeCheck("struct Session(const token: String);");
    try expectTypeCheck("case struct User(const id: Int, const name: String);");
}

test "struct construction type checks" {
    try expectTypeCheck(
        \\case struct User(const id: Int, const name: String);
        \\const u = User(1, "Alice");
    );
}

test "struct construction wrong arg count rejected" {
    try expectTypeError(
        \\case struct User(const id: Int, const name: String);
        \\const u = User(1);
    );
}

test "struct construction wrong arg type rejected" {
    try expectTypeError(
        \\case struct User(const id: Int, const name: String);
        \\const u = User("wrong", "Alice");
    );
}

test "struct field access type checks" {
    try expectTypeCheck(
        \\case struct User(const id: Int, const name: String);
        \\const u = User(1, "Alice");
        \\const n: String = u.name;
    );
}

test "struct field access unknown field rejected" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const result = helpers.typeCheck(
        \\case struct User(const id: Int);
        \\const u = User(1);
        \\const x = u.unknown;
    , arena.allocator());
    try std.testing.expectError(error.UndefinedField, result);
}

test "struct used as field type" {
    try expectTypeCheck(
        \\case struct User(const id: Int);
        \\struct Session(const user: User, var token: String);
    );
}

// -- Enums

test "enum declaration is valid" {
    try expectTypeCheck("enum Role { Admin, Member(const team: String), Guest, }");
}

test "enum variant construction type checks" {
    try expectTypeCheck(
        \\enum Role { Admin, Member(const team: String), Guest, }
        \\const r = Role.Member("eng");
    );
}

test "enum variant construction wrong arg count rejected" {
    try expectTypeError(
        \\enum Role { Admin, Member(const team: String), }
        \\const r = Role.Member("eng", "extra");
    );
}

test "enum variant construction wrong arg type rejected" {
    try expectTypeError(
        \\enum Role { Admin, Member(const team: String), }
        \\const r = Role.Member(42);
    );
}

test "enum variant field access type checks" {
    try expectTypeCheck(
        \\enum Role { Admin, Member(const team: String), }
        \\const r = Role.Member("eng");
        \\const t: String = r.team;
    );
}

test "enum composition declaration and explicit construction type checks" {
    try expectTypeCheck(
        \\enum StaffRole { Admin, Member(const team: String), }
        \\enum AnyRole { StaffRole, Guest, }
        \\const r = AnyRole.StaffRole(StaffRole.Admin());
    );
}

test "enum composition implicit coercion type checks" {
    try expectTypeCheck(
        \\enum StaffRole { Admin, Member(const team: String), }
        \\enum AnyRole { StaffRole, Guest, }
        \\const r: AnyRole = StaffRole.Admin();
    );
}

test "enum composition wrong type rejected" {
    try expectTypeError(
        \\enum StaffRole { Admin, }
        \\enum AnyRole { StaffRole, Guest, }
        \\const r: AnyRole = 42;
    );
}

// -- !T linearity

test "discarding !T result is a compile error" {
    try expectUnconsumedFallibleError(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() { fallible(); }
    );
}

test "unconsumed !T binding is a compile error" {
    try expectUnconsumedFallibleError(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() { const result = fallible(); }
    );
}

test "consumed !T binding via try is allowed" {
    try expectTypeCheck(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() E!Int { const result = fallible(); try result; }
    );
}

test "consumed !T binding via catch is allowed" {
    try expectTypeCheck(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() Int { const result = fallible(); result catch { -1; }; }
    );
}

test "explicit discard of !T is allowed" {
    try expectTypeCheck(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() { const _ = fallible(); }
    );
}

test "reassigning unconsumed !T var is a compile error" {
    try expectUnconsumedFallibleError(
        \\error enum E { Fail }
        \\fn f1() E!Int { 1; }
        \\fn f2() E!Int { 2; }
        \\fn main() E!Int { var result = f1(); result = f2(); try result; }
    );
}

test "reassigning consumed !T var is allowed" {
    try expectTypeCheck(
        \\error enum E { Fail }
        \\fn f1() E!Int { 1; }
        \\fn f2() E!Int { 2; }
        \\fn main() E!Int { var result = f1(); try result; result = f2(); try result; }
    );
}

test "return with unconsumed !T binding is a compile error" {
    try expectUnconsumedFallibleError(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() Int { const result = fallible(); return 42; }
    );
}

test "returning a !T binding consumes it" {
    try expectTypeCheck(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() E!Int { const result = fallible(); return result; }
    );
}

test "if branches must consume same !T bindings" {
    try expectUnconsumedFallibleError(
        \\error enum E { Fail }
        \\fn fallible() E!Int { 1; }
        \\fn main() E!Int {
        \\    const result = fallible();
        \\    if (true) { try result; } else { 0; }
        \\}
        );
        }

        test "if both branches consume !T is allowed" {
        try expectTypeCheck(
            \\error enum E { Fail }
            \\fn fallible() E!Int { 1; }
            \\fn main() E!Int {
            \\    const result = fallible();
            \\    if (true) { try result; } else { try result; }
            \\}
        );
        }

        test "if without else must not consume !T" {
        try expectUnconsumedFallibleError(
            \\error enum E { Fail }
            \\fn fallible() E!Int { 1; }
            \\fn main() E!Int {
            \\    const result = fallible();
            \\    if (true) { try result; }
            \\}
    );
}
