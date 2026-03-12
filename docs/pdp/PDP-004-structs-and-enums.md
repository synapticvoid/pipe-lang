# PDP-004: Structs & Enums

**Status:** Implemented
**Issues:** #

## Summary

User-defined composite types. Structs group named fields; enums model sum
types with optional payloads. `case struct` gives value semantics (structural
equality), plain `struct` gives identity semantics.

## Syntax

### Structs

```pipe
# Plain struct — identity equality, opaque formatting (<Session>)
struct Session(const token: Str);

# Case struct — structural equality, transparent formatting
case struct User(const id: Int, const name: Str);

# Long form with body fields, methods
case struct User(const id: Int) {
    const tag: Str = "user";

    fn id_plus(self: Self, n: Int) Int { self.id + n; }
    fn default() Self { User(0); }
}
```

Constructor fields require `const`/`var` and a type annotation. Body fields
require a default value and are excluded from equality and formatting.

Instance methods take `self: Self` as first parameter (stripped from call
site). Static methods omit it and are called as `Type.method()`.

### Construction and field access

```pipe
const u = User(1, "Alice");
u.name;             # "Alice"
u.name = "Bob";     # ok if var, error if const
User.default();     # static method call
u.id_plus(10);      # instance method call
```

### Enums

```pipe
enum Role { Admin, Member(const team: Str), Guest }

const r = Role.Admin();          # no-payload — () still required
const m = Role.Member("eng");
m.team;                          # "eng"
```

### Error enums

```pipe
error enum MathError { DivByZero, Overflow }
```

Identical to `enum` but flagged as an error type for `E!T` (see PDP-005).

### Enum composition

```pipe
enum StaffRole { Admin, Member(const team: Str) }
enum AnyRole { StaffRole, Guest }

# Explicit
const r = AnyRole.StaffRole(StaffRole.Admin());

# Implicit coercion (with type annotation)
const r: AnyRole = StaffRole.Admin();
```

A variant whose name matches a declared enum creates a nested wrapper.
For `error enum`, the nested enum must also be an `error enum`.

## Semantics

### Equality

- `struct` (plain): identity (same pointer)
- `case struct`: structural (constructor fields only, body fields excluded)
- `enum`: structural (variant name + field values, recursive through composition)

An `equals(self: Self, other: Self) Bool` method overrides default equality
for structs.

### Special methods

- `to_str(self: Self) Str` — overrides `print()` formatting
- `equals(self: Self, other: Self) Bool` — overrides `==` / `!=`

### Default formatting

- `struct`: `<TypeName>`
- `case struct`: `TypeName(field1=val1, ...)`  (body fields excluded)
- Enum: `EnumName.VariantName(field=val, ...)`

## Type Rules

- Constructor arg count and types must match fields
- Field access on undefined fields → compile-time error
- `const` field assignment → compile-time error
- `Self` resolves to enclosing struct type — only valid inside struct body
- `error enum` composition rejects non-error nested enums

## Out of Scope

- Enum methods
- Pattern matching / `when` on variants
- Generic structs / enums
- Interfaces / traits
- Destructuring

## Open Questions

- Should enum types support methods?
