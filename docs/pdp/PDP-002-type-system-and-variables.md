# PDP-002: Type System & Variables

**Status:** Partially Implemented
**Issues:** #

## Summary

Define Pipe's primitive types, variable declarations with `const`/`var`
mutability, and the type annotation and inference rules that bind them together.

## Syntax

### Primitive types

```pipe
Int       # 64-bit signed integer
Float     # 64-bit floating-point (not yet used at runtime)
Bool      # true / false
Str       # UTF-8 string
Byte      # unsigned 8-bit integer (0..255)
Unit      # absence of a meaningful value (implicit return of blocks/functions)
Any       # escape hatch — compatible with all types
```

### Variable declarations

```pipe
# With initializer — type inferred
const name = "Alice";
var count = 0;

# With explicit type annotation
const name: Str = "Alice";
var count: Int = 0;

# Annotation without initializer (type required)
var count: Int;
```

### Type annotations on functions

```pipe
fn add(a: Int, b: Int) Int {
    return a + b;
}
```

Type annotations on function parameters are mandatory. Return type is optional
— defaults to `Unit` when omitted.

## Semantics

### Primitive types

| Type    | Values                        | Truthy when          |
|---------|-------------------------------|----------------------|
| `Int`   | 64-bit signed integers        | `!= 0`              |
| `Float` | 64-bit IEEE 754               | (not yet at runtime) |
| `Bool`  | `true`, `false`               | `true`               |
| `Str`   | UTF-8 string literals         | non-empty            |
| `Byte`  | unsigned 8-bit (0..255)       | `!= 0`              |
| `Unit`  | implicit void value           | never                |
| `Any`   | any value                     | follows inner type   |
| `null`  | null literal                  | never                |

`null` and `Unit` are both falsy. All other types follow the truthy rules above.

### `Byte` — unsigned 8-bit integer

`Byte` represents a single unsigned byte (0..255). There is no byte literal
syntax — bytes are created via the `Byte()` constructor or integer literals
with a type annotation:

```pipe
const b = Byte(0x41);        # from hex
const b: Byte = 65;          # from decimal with annotation
```

`Byte` promotes to `Int` in arithmetic expressions. Assigning an `Int` outside
0..255 to a `Byte` is a runtime error.

Byte sequences are `List[Byte]`. Conversion between `Str` and `List[Byte]` uses
`to_bytes()` / `to_str()`.

### `Any` — escape hatch

`Any` is compatible with all types. It disables type checking for that binding,
making the programmer responsible for correctness. Intended for interop
boundaries and gradual typing, not general use.

```pipe
fn debug(value: Any) {
    print(value);
}
```

### `const` — immutable binding

`const` declares a binding that cannot be reassigned. The value itself may
contain mutable interior state (e.g. a struct with `var` fields), but the
binding cannot point to a different value.

```pipe
const x = 42;
x = 10;          # compile-time error: cannot reassign constant
```

### `var` — mutable binding

`var` declares a binding that can be reassigned. The new value must be
compatible with the original type.

```pipe
var x = 42;
x = 10;          # ok
x = "hello";     # compile-time error: type mismatch
```

### Type inference

When an initializer is present, the type is inferred from the expression:

```pipe
const x = 42;          # inferred as Int
const s = "hello";     # inferred as Str
const b = true;        # inferred as Bool
```

When both an annotation and an initializer are present, the inferred type must
be compatible with the annotation — a mismatch is a compile-time error.

A declaration with neither an annotation nor an initializer is a compile-time
error.

### Type compatibility

Two types are compatible when:
- They are the same primitive type, or
- They are the same named struct type, or
- They are the same named enum type, or
- Either side is `Any`

No implicit conversions exist between primitive types — `Int` and `Float` are
not interchangeable.

### Equality

- Primitives: value equality (`==`, `!=`)
- `case struct`: deep value equality (field-by-field)
- `struct` (plain): identity (address) equality
- Structs defining an `equals` method: dispatched to that method

## Type Rules

- Every binding must have a resolvable type — either via annotation or inference
- `const` bindings cannot be reassigned — enforced at compile time
- `var` reassignment must preserve type compatibility — enforced at compile time
- Function parameter annotations are mandatory
- Mismatched annotation and initializer types are a compile-time error

## Out of Scope

- User-defined generic types (`List[T]` syntax is built-in only — see PDP-009)
- `Float` runtime arithmetic (type exists, operations not yet wired)
- String interpolation
- Type aliases
- Union types beyond error unions (see error handling PDP)

## Design Decisions

- `Float` shares arithmetic operators with `Int`. Mixed `Int`/`Float`
  expressions auto-promote to `Float`.
- `null` is a temporary placeholder. A type-safe nullable system will replace
  it (separate PDP).

## Open Questions

- None.
