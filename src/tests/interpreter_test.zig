const std = @import("std");
const helpers = @import("helpers");

fn expectEval(cases: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    inline for (cases) |case| {
        var buf: [64]u8 = undefined;
        var eval = try helpers.evaluate(case[0], arena.allocator());
        defer eval.deinit();
        const actual = try std.fmt.bufPrint(&buf, "{f}", .{eval.value});
        try std.testing.expectEqualStrings(case[1], actual);
    }
}

test "arithmetic" {
    try expectEval(.{
        .{ "3 + 2 * 4;", "11" },
        .{ "10 - 3;", "7" },
        .{ "6 / 2;", "3" },
    });
}

test "comparison" {
    try expectEval(.{
        .{ "3 + 2 > 4;", "true" },
        .{ "3 + 2 == 4;", "false" },
        .{ "3 + 2 == 5;", "true" },
        .{ "1 != 2;", "true" },
        .{ "3 <= 3;", "true" },
        .{ "3 >= 4;", "false" },
    });
}

test "unary" {
    try expectEval(.{
        .{ "-5;", "-5" },
        .{ "--5;", "5" },
        .{ "-(3 + 2);", "-5" },
        .{ "!0;", "true" },
        .{ "!1;", "false" },
        .{ "!true;", "false" },
        .{ "!false;", "true" },
    });
}

test "variables" {
    try expectEval(.{
        .{ "var a: Int = 1; a = 5; a;", "5" },
        .{ "var a = 1; a = 5; a;", "5" }, // Type inference
        .{ "var a = 1; a = a + 2; a;", "3" },
    });
}

test "block expressions" {
    try expectEval(.{
        .{ "{ 5; }", "5" },
        .{ "{ var a = 5; a; }", "5" },
        .{ "{ var a = 3; var b = 2; a + b; }", "5" },
    });
}

test "if expressions" {
    try expectEval(.{
        .{ "var a = if true { 5; } else { -5; }; a;", "5" },
        .{ "var a = if false { 5; } else { -5; }; a;", "-5" },
        .{ "var a = if (true) { 5; } else { -5; }; a;", "5" },
    });
}

test "function declaration and call" {
    try expectEval(.{
        .{ "fn add(a: Int, b: Int) Int { a + b; } add(1, 2);", "3" },
        .{ "fn double(x: Int) Int { x * 2; } double(5);", "10" },
        .{ "fn greet() Int { 42; } greet();", "42" },
    });
}

fn expectOutput(cases: anytype) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    inline for (cases) |case| {
        var eval = try helpers.evaluate(case[0], arena.allocator());
        defer eval.deinit();
        try std.testing.expectEqualStrings(case[1], eval.output);
    }
}

test "print" {
    try expectOutput(.{
        .{ "print(42);", "42\n" },
        .{ "print(\"hello\");", "hello\n" },
        .{ "print(1, 2, 3);", "1 2 3\n" },
        .{ "print(\"hello\", \"world\");", "hello world\n" },
    });
}

test "return statement" {
    try expectEval(.{
        .{ "fn five() Int { return 5; } five();", "5" },
        .{ "fn add(a: Int, b: Int) Int { return a + b; } add(1, 2);", "3" },
        .{ "fn early(x: Int) Int { if x > 0 { return x; } else { return 0; } } early(5);", "5" },
        .{ "fn early(x: Int) Int { if x > 0 { return x; } else { return 0; } } early(-1);", "0" },
    });
}

test "function with closure" {
    try expectEval(.{
        .{ "var x = 10; fn addX(a: Int) Int { a + x; } addX(5);", "15" },
    });
}

// -- try / catch

test "catch passes through ok value" {
    try expectEval(.{
        .{
            \\error enum E { Fail, }
            \\fn maybe(x: Int) E!Int { x; }
            \\maybe(42) catch e { 0; };
            ,
            "42",
        },
    });
}

test "try unwraps ok value" {
    try expectEval(.{
        .{
            \\error enum E { Fail, }
            \\fn maybe(x: Int) E!Int { x; }
            \\fn caller(x: Int) E!Int { try maybe(x); }
            \\caller(42);
            ,
            "E!Int.Ok(value=42)",
        },
    });
}

test "catch handles error" {
    try expectEval(.{
        .{
            \\error enum E { Fail, }
            \\fn fail() E!Int { E.Fail(); }
            \\fail() catch e { 0; };
            ,
            "0",
        },
    });
}

test "catch with binding exposes error" {
    try expectEval(.{
        .{
            \\error enum E { Fail(const code: Int), }
            \\fn fail() E!Int { E.Fail(99); }
            \\fail() catch e { e.code; };
            ,
            "99",
        },
    });
}

test "catch one-liner with binding handles error" {
    try expectEval(.{
        .{
            \\error enum E { Fail(const code: Int), }
            \\fn fail() E!Int { E.Fail(99); }
            \\fail() catch e => e.code;
            ,
            "99",
        },
    });
}

test "try propagates error out of function" {
    try expectEval(.{
        .{
            \\error enum E { Fail, }
            \\fn fail() E!Int { E.Fail(); }
            \\fn caller() E!Int { try fail(); }
            \\caller();
            ,
            "E!Int.Err(err=E.Fail())",
        },
    });
}

// -- Structs

test "case struct construction and field access" {
    try expectEval(.{
        .{ "case struct User(const id: Int, const name: String); const u = User(1, \"Alice\"); u.id;", "1" },
        .{ "case struct User(const id: Int, const name: String); const u = User(1, \"Alice\"); u.name;", "\"Alice\"" },
    });
}

test "case struct formatting" {
    try expectEval(.{
        .{ "case struct User(const id: Int, const name: String); User(1, \"Alice\");", "User(id=1, name=\"Alice\")" },
    });
}

test "case struct equality is structural" {
    try expectEval(.{
        .{ "case struct Point(const x: Int, const y: Int); Point(1, 2) == Point(1, 2);", "true" },
        .{ "case struct Point(const x: Int, const y: Int); Point(1, 2) == Point(3, 4);", "false" },
        .{ "case struct Point(const x: Int, const y: Int); Point(1, 2) != Point(3, 4);", "true" },
    });
}

test "plain struct formatting" {
    try expectEval(.{
        .{ "struct Session(const token: String); Session(\"abc\");", "<Session>" },
    });
}

test "plain struct equality is by identity" {
    try expectEval(.{
        .{ "struct S(const x: Int); const a = S(1); const b = S(1); a == b;", "false" },
        .{ "struct S(const x: Int); const a = S(1); a == a;", "true" },
    });
}

test "struct field access in expressions" {
    try expectEval(.{
        .{ "case struct Point(const x: Int, const y: Int); const p = Point(3, 4); p.x + p.y;", "7" },
    });
}

test "struct passed to function" {
    try expectEval(.{
        .{ "case struct Point(const x: Int, const y: Int); fn sum(p: Point) Int { p.x + p.y; } sum(Point(3, 4));", "7" },
    });
}

test "struct field assignment" {
    try expectEval(.{
        .{
            \\case struct User(const id: Int, var name: String);
            \\var u = User(1, "Alice");
            \\u.name = "Bob";
            \\u.name;
            ,
            "\"Bob\"",
        },
    });
}

test "struct body field with default" {
    try expectEval(.{
        .{
            \\case struct User(const id: Int) {
            \\    const tag: String = "user";
            \\}
            \\const u = User(1);
            \\u.tag;
            ,
            "\"user\"",
        },
    });
}

test "struct body field excluded from toString" {
    try expectOutput(.{
        .{
            \\case struct User(const id: Int) {
            \\    const tag: String = "user";
            \\}
            \\print(User(1));
            ,
            "User(id=1)\n",
        },
    });
}

test "struct body field excluded from equals" {
    try expectEval(.{
        .{
            \\case struct User(const id: Int) {
            \\    var tag: String = "a";
            \\}
            \\const a = User(1);
            \\const b = User(1);
            \\a == b;
            ,
            "true",
        },
    });
}

test "struct print" {
    try expectOutput(.{
        .{ "case struct User(const id: Int, const name: String); print(User(1, \"Alice\"));", "User(id=1, name=\"Alice\")\n" },
        .{ "struct Session(const token: String); print(Session(\"abc\"));", "<Session>\n" },
    });
}

test "struct toString override" {
    try expectOutput(.{
        .{
            \\case struct User(const id: Int, const name: String) {
            \\    fn toString(self: Self) String { self.name; }
            \\}
            \\print(User(1, "Alice"));
            ,
            "Alice\n",
        },
    });
}

// -- Struct methods

test "struct instance method call" {
    try expectEval(.{
        .{
            \\case struct User(const id: Int) {
            \\    fn id_plus(self: Self, n: Int) Int { self.id + n; }
            \\}
            \\const u = User(1);
            \\u.id_plus(10);
            ,
            "11",
        },
    });
}

test "struct static method call" {
    try expectEval(.{
        .{
            \\case struct User(const id: Int) {
            \\    fn default() Self { User(0); }
            \\}
            \\User.default().id;
            ,
            "0",
        },
    });
}

test "struct equals override" {
    try expectEval(.{
        .{
            \\case struct User(const id: Int, const name: String) {
            \\    fn equals(self: Self, other: Self) Bool { self.id == other.id; }
            \\}
            \\const a = User(1, "Alice");
            \\const b = User(1, "Bob");
            \\a == b;
            ,
            "true",
        },
        .{
            \\case struct User(const id: Int, const name: String) {
            \\    fn equals(self: Self, other: Self) Bool { self.id == other.id; }
            \\}
            \\User(1, "Alice") == User(2, "Alice");
            ,
            "false",
        },
        .{
            \\case struct User(const id: Int, const name: String) {
            \\    fn equals(self: Self, other: Self) Bool { self.id == other.id; }
            \\}
            \\User(1, "Alice") != User(1, "Bob");
            ,
            "false",
        },
    });
}

// -- Enums

test "enum no-payload variant construction" {
    try expectEval(.{
        .{ "enum Role { Admin, Member, Guest, } Role.Admin();", "Role.Admin()" },
    });
}

test "enum variant with field construction and access" {
    try expectEval(.{
        .{ "enum Role { Admin, Member(const team: String), Guest, } const r = Role.Member(\"eng\"); r.team;", "\"eng\"" },
    });
}

test "enum variant print" {
    try expectOutput(.{
        .{ "enum Role { Admin, Member(const team: String), } print(Role.Admin());", "Role.Admin()\n" },
        .{ "enum Role { Admin, Member(const team: String), } print(Role.Member(\"eng\"));", "Role.Member(team=\"eng\")\n" },
    });
}

test "enum equality is structural" {
    try expectEval(.{
        .{ "enum Role { Admin, Member(const team: String), } Role.Admin() == Role.Admin();", "true" },
        .{ "enum Role { Admin, Member(const team: String), } Role.Member(\"eng\") == Role.Member(\"eng\");", "true" },
        .{ "enum Role { Admin, Member(const team: String), } Role.Member(\"eng\") == Role.Member(\"other\");", "false" },
        .{ "enum Role { Admin, Member(const team: String), } Role.Admin() == Role.Member(\"eng\");", "false" },
    });
}

test "enum composition explicit construction" {
    try expectEval(.{
        .{ "enum StaffRole { Admin, Member(const team: String), } enum AnyRole { StaffRole, Guest, } AnyRole.StaffRole(StaffRole.Admin());", "AnyRole.StaffRole(StaffRole=StaffRole.Admin())" },
        .{ "enum StaffRole { Admin, Member(const team: String), } enum AnyRole { StaffRole, Guest, } AnyRole.StaffRole(StaffRole.Member(\"eng\"));", "AnyRole.StaffRole(StaffRole=StaffRole.Member(team=\"eng\"))" },
    });
}

test "enum composition implicit coercion" {
    try expectEval(.{
        .{ "enum StaffRole { Admin, } enum AnyRole { StaffRole, Guest, } const r: AnyRole = StaffRole.Admin(); r;", "StaffRole.Admin()" },
    });
}

test "enum composition equality" {
    try expectEval(.{
        .{ "enum StaffRole { Admin, } enum AnyRole { StaffRole, Guest, } const a: AnyRole = StaffRole.Admin(); const b: AnyRole = StaffRole.Admin(); a == b;", "true" },
        .{ "enum StaffRole { Admin, Guest, } enum AnyRole { StaffRole, } const a: AnyRole = StaffRole.Admin(); const b: AnyRole = StaffRole.Guest(); a == b;", "false" },
    });
}

test "enum composition print" {
    try expectOutput(.{
        .{ "enum StaffRole { Admin, } enum AnyRole { StaffRole, Guest, } print(AnyRole.StaffRole(StaffRole.Admin()));", "AnyRole.StaffRole(StaffRole=StaffRole.Admin())\n" },
        .{ "enum StaffRole { Admin, } enum AnyRole { StaffRole, Guest, } print(AnyRole.Guest());", "AnyRole.Guest()\n" },
    });
}
