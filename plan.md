# Pipe Language — Type System Design

## Implementation plan

Shortest path to error payloads without breaking changes later.
Each phase is self-contained and testable.

### Phase 1: Lexer foundation
- [ ] Add `struct` keyword token
- [ ] Add `union` keyword token
- [ ] Add `case` keyword token
- [ ] Add `when` keyword token (reserved for future pattern matching)
- [ ] Add `self` keyword token (reserved for instance methods)
- [ ] Add `Self` keyword token (reserved for self type annotation)
- [ ] Add `.` (dot) single-character token
- [ ] Tests: lex `struct`, `union`, `case`, `when`, `self`, `Self`, `.`

### Phase 2: Struct (short form, no methods)
- [ ] AST: `Statement.struct_declaration` (name, fields with mutability, case flag)
- [ ] AST: `Expression.struct_init` (type name, field initializers with `=`)
- [ ] AST: `Expression.field_access` (object expression, field name)
- [ ] Parser: parse `struct Session(const user: User, var token: String);` (`;` terminates short form)
- [ ] Parser: parse `case struct User(const id: Int, const name: String, var email: String);`
- [ ] Parser: parse `User(1, "Alice", "alice@example.com")` as struct construction (positional args, parsed as call, disambiguated in type checker)
- [ ] Parser: parse `expr.field` as dot access (uniform — field access, method calls, and qualified construction all parse the same; semantics resolved in type checker)
- [ ] Types: add `PipeType.struct_type` (name → field descriptors + case flag)
- [ ] Type checker: register struct types, check construction and field access
- [ ] Interpreter: `Value.struct_instance`, evaluate construction and field access
- [ ] `case struct`: auto-derived structural `==` and `toString` → `User(id=1, name=Alice, email=alice@example.com)`
- [ ] Plain `struct`: identity `==` and `toString` → `<Session>`
- [ ] Tests: declare, construct, access fields, print, compare (both case and plain)

### Phase 3: Union
- [ ] AST: `Statement.union_declaration` (name, variants with optional fields)
- [ ] Parser: parse `union Role { Admin, Member(const team: String), Guest }`
- [ ] Parser: parse `Role.Member("engineering")` as qualified construction (reuses dot access from Phase 2)
- [ ] Types: add union type to registry (variant name → optional field descriptors)
- [ ] Type checker: validate union construction
- [ ] Interpreter: evaluate union construction, support field access on variants
- [ ] Unions are always structural: `==` compares variant tag + fields, `toString` shows `Role.Member(team=engineering)`
- [ ] Union composition: `union AnyRole { StaffRole, Guest }` (flatten existing unions)
- [ ] Tests: declare, construct, access variant fields, compare, compose unions

### Phase 4: Error payloads
- [ ] Evolve error syntax: `error union UserError { NotFound(const id: Int), Unknown }`
- [ ] Reuse union machinery — `error union` is a union that can appear in `!T` position
- [ ] Variants with fields become struct-like payloads
- [ ] Error values carry payload (accessible via field access after catch)
- [ ] Breaking change: migrate existing `error Name { V1, V2 }` to `error union`
- [ ] Tests: error with payload, catch and access fields

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
- **Unions are always structural**: `==` compares variant tag + field values, `toString` auto-derived. No `case` modifier for unions.
- **No `const struct` / `const union`**: mutability is always per-field, no type-level shorthand
- **Positional construction for now**: `Point(1, 2)` — keyword arguments deferred as a separate feature for both constructors and function calls
- **Struct construction looks like a function call**: parsed as a call, disambiguated in the type checker
- **Dot access is uniform**: `expr.name` is parsed the same for field access, method calls, and qualified variant construction — semantics resolved in the type checker
- **Union variant fields use `var`/`const`**: same rule as struct fields — scripting pragmatism over FP purity
- **Single `{}` syntax for unions**: no `|` composition — `union AppError { IOError, ValidationError }` flattens existing unions
- **`error union` reuses union machinery**: an `error union` is a union that can appear in `!T` position — breaking change from existing `error Name { V1, V2 }`
- **`when`, `self`, `Self` reserved now**: keywords added in Phase 1, used in later phases
- **No `@identity` needed**: plain `struct` already provides identity semantics; `case struct` provides structural
- **Declaration termination**: `}` if it has braces, `;` otherwise — no `};`
- **`toString` / `equals`**: well-known method names, no special syntax
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

### Union

```pipe
union Role {
    Admin,
    Member(const team: String),
    Guest,
}

// Composing unions — flattens existing unions into one
union StaffRole {
    Admin,
    Member(const team: String),
}

union AnyRole {
    StaffRole,
    Guest,
}
```

**Construction & access:**
```pipe
const role = Role.Member("engineering");
print(role.team);    // engineering

// Pattern matching (future)
when role {
    Role.Admin -> print("admin"),
    Role.Member(team) -> print(team),
    Role.Guest -> print("guest"),
}
```

### Error union

```pipe
// Declaring — same as union but allowed in !T position
error union UserError {
    NotFound(const id: Int),
    PermissionDenied(const role: String),
    Unknown,
}

// Composing
error union AppError {
    UserError,
    ValidationError,
}

// Usage in return types (existing !T syntax)
fn find_user(id: Int) UserError!User { ... }
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

### Full example

```pipe
case struct User(const id: Int, const name: String, var email: String);

error union UserError {
    NotFound(const id: Int),
    Unknown,
}

fn find_user(id: Int) UserError!User {
    // ...
}

const user = find_user(42) catch |e| {
    print(e.id);
    return;
};
print(user);            // User(id=42, name=Alice, email=alice@example.com)
print(user == user);    // true (case struct → structural ==)
print(user.name);       // Alice
```
