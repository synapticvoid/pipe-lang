# Pipe Language — Type System Design

## Implementation plan

Shortest path to error payloads without breaking changes later.
Each phase is self-contained and testable.

### Phase 1: Lexer foundation ✅
- [x] Add `struct` keyword token
- [x] Add `union` keyword token (renamed to `enum` in Phase 3.5)
- [x] Add `case` keyword token
- [x] Add `when` keyword token (reserved for future pattern matching)
- [x] Add `self` keyword token (reserved for instance methods)
- [x] Add `Self` keyword token (reserved for self type annotation)
- [x] Add `.` (dot) single-character token
- [x] Tests: lex `struct`, `union`, `case`, `when`, `self`, `Self`, `.` (update `union` → `enum` in Phase 3.5)

### Phase 2: Struct (short form, no methods) ✅
- [x] AST: `Statement.struct_declaration` (name, fields with mutability, case flag)
- [x] AST: `Expression.struct_init` (type name, field initializers with `=`)
- [x] AST: `Expression.field_access` (object expression, field name)
- [x] Parser: parse `struct Session(const user: User, var token: String);` (`;` terminates short form)
- [x] Parser: parse `case struct User(const id: Int, const name: String, var email: String);`
- [x] Parser: parse `User(1, "Alice", "alice@example.com")` as struct construction (positional args, parsed as call, disambiguated in type checker)
- [x] Parser: parse `expr.field` as dot access (uniform — field access, method calls, and qualified construction all parse the same; semantics resolved in type checker)
- [x] Types: add `PipeType.struct_type` (name → field descriptors + case flag)
- [x] Type checker: register struct types, check construction and field access
- [x] Interpreter: `Value.struct_instance`, evaluate construction and field access
- [x] `case struct`: auto-derived structural `==` and `toString` → `User(id=1, name=Alice, email=alice@example.com)`
- [x] Plain `struct`: identity `==` and `toString` → `<Session>`
- [x] Tests: declare, construct, access fields, print, compare (both case and plain)

### Phase 3: Enum (née union) ✅
- [x] AST: `Statement.union_declaration` (name, variants with fields as non-optional slice)
- [x] Parser: parse `union Role { Admin, Member(const team: String), Guest }`
- [x] Parser: parse `Role.Member("engineering")` as qualified construction (reuses dot access from Phase 2)
- [x] Types: add `PipeType.union_type`, `TypeInfo` registry (struct and union share one hashmap), `UnionTypeInfo`, `VariantTypeInfo`
- [x] Type checker: register union type + variant constructors, validate construction and field access
- [x] Interpreter: evaluate union construction, support field access on variants
- [x] Unions are always structural: `==` compares variant tag + fields, `toString` shows `Role.Member(team=engineering)`
- [x] Union composition: `union AnyRole { StaffRole, Guest }` (nests existing unions as variants)
- [x] Tests: declare, construct, access variant fields, compare, compose unions

### Phase 3.5: Rename `union` → `enum` ✅
- [x] Lexer: replace `union` keyword token with `enum`
- [x] AST: rename `union_declaration` → `enum_declaration`, `union_type` → `enum_type` throughout
- [x] Parser, type checker, interpreter: mechanical rename to match
- [x] Types: rename `PipeType.union_type` → `PipeType.enum_type`, `UnionTypeInfo` → `EnumTypeInfo`, `VariantTypeInfo` stays
- [x] Update all tests and syntax references
- [x] Remove vestigial `PipeType.error_set` and `PipeType.error_union` variants — deferred to Phase 4 (still power existing try/catch system)

### Phase 4: Error payloads & errors as values
- [x] Lexer: add `try` and `catch` keywords (`error` already exists — verify and keep)
- [x] AST: add `is_error: bool` to `EnumDeclaration` — no new node needed
- [x] AST: add `Expression.try_expr`, `Expression.catch_expr` (binding name + body)
- [x] Parser: `error enum Name { ... }` → sets `is_error = true`, otherwise identical to `enum`
- [x] Parser: `try expr`, `expr catch |e| { ... }`
- [x] Types: add `is_error: bool` to `EnumTypeInfo` — no new type variant needed
- [x] Types: remove vestigial `PipeType.error_set` and `PipeType.error_union` — superseded by synthesized enum
- [x] Type checker: propagate `is_error` when registering; enforce that composed variants inside an `error enum` are themselves `error enum`s
- [x] Type checker: validate `!T` error side must be an `is_error` enum
- [x] Type checker: `E!T` synthesizes a concrete `EnumTypeInfo` with two variants — `Ok` (fields: `[T]`) and `Err` (fields: `[E]`) — no new PipeType variant needed
- [x] Type checker: `try` — caller must also return `!T`, propagates error type upward; `catch` — valid on fallible expression only; rejected on non-fallible
- [x] Type checker: `!T` is a first-class type — a `!T` value cannot be used where `T` is expected without unwrapping via `try`, `catch`, or `when`
- [x] Type checker: discarding a `!T` result with no binding (`foo();`) is a compile error; explicit discard (`const _ = foo()`) is allowed
- [x] Type checker: scope-level pending-set tracking — each scope maintains a set of unresolved `!T` bindings; all must be consumed before scope exit
- [x] Type checker: branch merge — each branch of `if`/`when` gets a copy of the pending set; both branches must consume the same bindings at the merge point
- [x] Type checker: early exit (`return`, `break`, `continue`) — all pending `!T` bindings must be consumed before the exit point
- [x] Type checker: reassignment — overwriting a `var` binding holding an unconsumed `!T` is a compile error
- [x] Type checker: consumption — `try`, `catch`, `when`, `return res`, passing as argument, and rebinding (`const other = res`) all consume a `!T` binding; rebinding transfers the obligation to the new name
- [x] Interpreter: `!T` values are `Value.enum_instance` — `Ok` variant wraps the success value, `Err` variant wraps the error; no new Value variants needed; `result_name` computed in `executeFnDeclarationStatement`
- [x] Interpreter: `try` — match on `Ok`/`Err` variant, unwrap or propagate; `catch` — match on `Ok`/`Err`, unwrap or bind and execute handler (stubbed as `NotImplemented`)
- [x] Tests: declare error enum, catch and access fields, try propagates error, catch with binding, unconsumed binding errors, branch merge errors, reassignment error

### Phase 5: Struct methods
- [ ] AST: methods list in struct declaration (reuse `FnDeclaration`)
- [ ] Parser: parse `fn` inside struct body
- [ ] `Self` type resolves to enclosing struct
- [ ] Instance methods: `self` as first param, called via `expr.method(args)`
- [ ] `self: const Self` for read-only methods (cannot mutate `var` fields)
- [ ] Static methods: no `self`, called via `Type.method(args)`
- [ ] Override hooks: `fn toString(self: const Self) String`, `fn equals(self: const Self, other: const Self) Bool`
- [ ] Tests: instance methods, static methods, const/mut self, toString/equals override

### Phase 6: Mutability & long form
- [ ] Per-field `const`/`var` enforcement (prevent assignment to `const` fields)
- [ ] Long form struct with computed defaults (`const created_at: Int = now();`)
- [ ] `init {}` guard block (future — validation only, no field assignment, no `self`)
- [ ] Tests: const enforcement, computed defaults

---

## Design decisions

- **`case struct` vs plain `struct`**: `case` enables auto-derived structural `==` and `toString`. Plain `struct` uses identity `==` and shows `<TypeName>`.
- **`union` renamed to `enum`**: `union` implies C-style untagged union; pipe-lang's type is a tagged sum type (algebraic data type), which is what Rust, Swift, and Kotlin call `enum`. Renamed in Phase 3.5.
- **Enums are always structural**: `==` compares variant tag + field values, `toString` auto-derived. No `case` modifier for enums.
- **No `const struct` / `const enum`**: mutability is always per-field, no type-level shorthand
- **Positional construction for now**: `Point(1, 2)` — keyword arguments deferred as a separate feature for both constructors and function calls
- **Struct construction looks like a function call**: parsed as a call, disambiguated in the type checker
- **Dot access is uniform**: `expr.name` is parsed the same for field access, method calls, and qualified variant construction — semantics resolved in the type checker
- **Union variant fields use `var`/`const`**: same rule as struct fields — scripting pragmatism over FP purity
- **Enum composition nests, not flattens**: `enum AppError { UserError, ValidationError }` makes `UserError` a variant wrapping the inner enum — enables grouped pattern matching (`AppError.UserError(_)` catches all user errors)
- **Errors are values**: fallible functions return a result wrapper, not a control-flow signal. `try`, `catch`, and `when` are the three ways to handle them. Enforcement is at the type level, not the call site — a `!T` value cannot be used as `T` without unwrapping.
- **`!T` is a synthesized enum**: `E!T` is syntax sugar — the type checker synthesizes a concrete `EnumTypeInfo` with `Ok(T)` and `Err(E)` variants. No new PipeType variant needed; all enum machinery (dispatch, `when` matching, interpreter values) works as-is. `value =>` / `error =>` arm syntax in `when` is sugar for matching `Ok` / `Err` variants.
- **`!T` consumption is tracked**: the type checker maintains a pending-set of unresolved `!T` bindings per scope. All bindings must be consumed (via `try`, `catch`, `when`, `return`, passing as argument, or rebinding) before scope exit, branch merge, or early exit. Discarding a `!T` result with no binding is a compile error. Reassigning over an unconsumed `!T` binding is a compile error.
- **`error enum` reuses enum machinery**: an `error enum` is an enum with `is_error: bool` set — no new type variant, no parallel registry path. Only difference: can appear in `!T` position, and composed variants must themselves be `error enum`s.
- **`is_error` is a marker, not a kind**: analogous to Swift's `Error` protocol conformance or Kotlin's sealed modifier — a boolean flag on `EnumTypeInfo`, not a separate type kind. A kind enum would be over-engineering for a single distinction.
- **Vestigial `error_set` / `error_union` removed**: predated the enum infrastructure; superseded by `is_error` on `EnumTypeInfo`.
- **`when`, `self`, `Self` reserved now**: keywords added in Phase 1, used in later phases
- **No `@identity` needed**: plain `struct` already provides identity semantics; `case struct` provides structural
- **Declaration termination**: `}` if it has braces, `;` otherwise — no `};`
- **`toString` / `equals`**: well-known method names, no special syntax
- **`const` binding freezes everything**: a `const` binding prevents mutation of `var` fields — the binding controls mutability, not the field declaration. `var` fields are only mutable through a `var` binding.
- **`when`**: see `plan_when.md` for full design (expression, exhaustive, works on any type)
- **Implicit coercion for nested unions**: `const r: AnyRole = StaffRole.Admin;` works — a child union value can be assigned where a parent union is expected
- **Generics**: planned for later, not in scope for these phases
- **`const Self` for read-only methods**: `fn foo(self: const Self)` prevents mutation of `var` fields — like C++ `const` methods
- **No `init` constructor**: computed defaults handle derived fields; `init {}` guard block (future) is validation-only, no `self`, no field assignment. Complex construction uses static methods.

---

## Syntax reference

All examples use a consistent User/Role domain.

### Struct

```pipe
// case struct — auto-derived == and toString
case struct User(const id: Int, const name: String, var email: String);
// print → User(id=1, name=Alice, email=alice@example.com)
// == compares fields

// Plain struct — identity == and opaque toString
struct Session(const user: User, var token: String);
// print → <Session>
// == compares identity
```

**Defaults:**
- Every field must specify `var` or `const` — no implicit defaults
- `case struct`: structural `==`, auto `toString` with field dump
- Plain `struct`: identity `==`, `toString` shows `<TypeName>`

**Override hooks (well-known method names):**
- `fn equals(self: const Self, other: const Self) Bool` → overrides `==`
- `fn toString(self: const Self) String` → overrides print formatting

### Enum

```pipe
enum Role {
    Admin,
    Member(const team: String),
    Guest,
}

// Composing enums — nests existing enums as variants
enum StaffRole {
    Admin,
    Member(const team: String),
}

enum AnyRole {
    StaffRole,   // nested: AnyRole.StaffRole wraps the StaffRole enum
    Guest,
}
```

**Construction & access:**
```pipe
const role = Role.Member("engineering");
print(role.team);    // engineering
```

### Error enum

```pipe
// Declaring — same as enum but allowed in !T position
error enum UserError {
    NotFound(const id: Int),
    PermissionDenied(const role: String),
    Unknown,
}

// Composing — nested variants must also be error enums
error enum AppError {
    UserError,        // ✅ UserError is an error enum
    ValidationError,  // ✅ ValidationError is an error enum
    // SomeRegularEnum  ❌ compile error — not an error enum
}

// Usage in return types (existing !T syntax)
fn find_user(id: Int) UserError!User { ... }   // UserError must be an error enum
```

### Construction (uniform)

```pipe
// Positional args — keyword arguments deferred as a separate feature
const alice = User(1, "Alice", "alice@example.com");
const session = Session(alice, "tok_abc123");
const role = Role.Member("engineering");
```

### Methods

```pipe
case struct User(const id: Int, const name: String, var email: String) {
    fn display(self: const Self) String {
        self.name + " <" + self.email + ">"
    }

    fn anonymous() Self {
        User(0, "anonymous", "")
    }
}

const alice = User(1, "Alice", "alice@example.com");
alice.display();       // Alice <alice@example.com>
User.anonymous();      // User(id=0, name=anonymous, email=)
```

### Long form struct

```pipe
struct Session(const user: User, var token: String) {
    const created_at: Int = now();
    const TOKEN_PREFIX: String = "tok_";

    fn toString(self: const Self) String {
        return self.user.name + " session";
    }
}
```

### Error handling

```pipe
// Errors are values — three ways to unwrap a !T:

// 1. try — unwrap or propagate to caller (caller must also return !T)
const user = try find_user(42);

// 2. catch — handle inline
const user = find_user(42) catch |e| {
    print(e);
    return;
};

// 3. when — see plan_when.md
when (find_user(42)) {
    user  => print(user.name),
    error => print("failed"),
}

// !T can be bound and handled later — res has type !User
const res = find_user(42);
when (res) {
    user  => print(user.name),
    error => print("failed"),
}

// Discarding a !T result with no binding → compile error
find_user(42);       // ERROR: result discarded
const _ = find_user(42);  // OK — explicit discard

// Unconsumed !T binding → compile error
const res = find_user(42);
// scope ends here without consuming res → ERROR

// Partial handling → compile error
const res = find_user(42);
if (cond) {
    when (res) { ... }  // consumed in this branch
}
// else branch does not consume res → ERROR
```

### Full example

```pipe
case struct User(const id: Int, const name: String, var email: String);

error enum UserError {
    NotFound(const id: Int),
    Unknown,
}

fn find_user(id: Int) UserError!User {
    // ...
}

const user = find_user(42) catch |e| {
    print(e);
    return;
};
print(user);            // User(id=42, name=Alice, email=alice@example.com)
print(user == user);    // true (case struct → structural ==)
print(user.name);       // Alice
```
