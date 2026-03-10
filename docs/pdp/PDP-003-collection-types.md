# PDP-003: Collection Types

**Status:** Draft
**Issues:** #33, #34, #35

## Summary

Introduce three built-in collection types — `List[T]`, `Map[K, V]`, and `Set[T]`
— with Python-style literals, a uniform method surface, and method-call sugar
over collection builtins. All types in Pipe are PascalCase; these built-ins are
no exception.

## Syntax

### Type syntax

```pipe
List[T]
Map[K, V]
Set[T]
```

### Literals

```pipe
# List
const xs: List[Int] = []           # empty — annotation required
const xs = [1, 2, 3]               # inferred as List[Int]

# Map
const m: Map[Str, Int] = {}        # empty — annotation required
const m = {"a": 1, "b": 2}         # inferred as Map[Str, Int]

# Set
const s: Set[Int] = #{}            # empty — annotation required
const s = #{1, 2, 3}               # inferred as Set[Int]
```

### Method syntax

```pipe
xs.len()                            # -> Int
xs.contains(value)                  # -> Bool
xs.get(index)                       # -> IndexError!T   (List, error on out-of-bounds)
xs.get(index, default)              # -> T              (List, returns default)
xs.add(value)                       # -> Unit           (mutating)
xs.remove(index)                    # -> IndexError!T   (mutating)

# Map
m.get(key)                          # -> KeyError!V
m.get(key, default)                 # -> V
m.add(key, value)                   # -> Unit           (mutating, insert or replace)
m.remove(key)                       # -> KeyError!V     (mutating)

# Set
s.get(value)                        # -> Bool           (alias for contains)
s.add(value)                        # -> Unit           (mutating, duplicate silently ignored)
s.remove(value)                     # -> Bool           (mutating, returns whether it was present)
s.union(other)                      # -> Set[T]
s.intersection(other)               # -> Set[T]
s.difference(other)                 # -> Set[T]
```

## Semantics

### `List[T]`

An ordered, indexable sequence of values of type `T`. Preserves insertion order.
Index access is zero-based.

Negative indexing is supported: `xs[-1]` is equivalent to `xs[xs.len() - 1]`.
Negative out-of-bounds is `IndexError`, same as positive out-of-bounds.

### `Map[K, V]`

An unordered key-value store. No iteration order is guaranteed. `K` must be
hashable (see Hashability below).

Ordered map variants (`OrderedMap`, `SortedMap`) are stdlib types, available
once generics are implemented. They are out of scope for this PDP.

### `Set[T]`

An unordered collection of unique values of type `T`. `T` must be hashable.
Duplicate values are silently ignored on `add`.

The `#{}` sigil distinguishes set literals from map literals (`{}`), including
the empty case. This follows Clojure's precedent and avoids any parser
ambiguity.

### Method-call sugar

Method calls on collection values desugar to builtin calls:

| Method form         | Desugars to          |
|---------------------|----------------------|
| `xs.len()`          | `len(xs)`            |
| `xs.contains(v)`    | `contains(xs, v)`    |
| `xs.get(i)`         | `get(xs, i)`         |
| `xs.get(i, d)`      | `get(xs, i, d)`      |
| `xs.add(v)`         | `add(xs, v)`         |
| `xs.remove(i)`      | `remove(xs, i)`      |

Sugar only — behavior and types match the builtin forms exactly.

### Mutability

`const` forbids rebinding the variable. Mutating methods (`add`, `remove`)
require a mutable receiver (`var`). Using a mutating method on a `const`
binding is a compile-time type error.

```pipe
const xs = [1, 2, 3]
xs.add(4)               # type error: mutating method on const binding

var ys = [1, 2, 3]
ys.add(4)               # ok
```

### Type inference

Non-empty literals infer their element type from their contents:
- `[1, 2, 3]` → `List[Int]`
- `{"a": 1}` → `Map[Str, Int]`
- `#{1, 2, 3}` → `Set[Int]`

Empty literals (`[]`, `{}`, `#{}`) require an explicit type annotation.
Using an empty literal without annotation is a compile-time error.

Mixed-type literals are a type error. No implicit `Any` inference.

### Hashability

A type `T` is hashable if:
- It is a built-in primitive (`Int`, `Str`, `Bool`, `Float`), or
- It defines a `hash() -> Int` method

This mirrors the existing `toString()` convention: built-in implementations
exist for primitives; structs opt in by defining `hash()`.

Equality (`==`) is always defined for all types:
- Primitives: value equality
- Regular structs: identity (address) equality by default
- Case structs, or structs defining `eql() -> Bool`: value equality

A regular struct without `hash()` is still hashable via address — enabling
identity-based sets and maps. A struct with `hash()` should also define
`eql()` for consistent value semantics; the compiler may warn if only one
is defined.

Using a non-hashable type as `K` in `Map[K, V]` or `T` in `Set[T]` is a
compile-time type error.

### Set operations

`union`, `intersection`, and `difference` are defined in this PDP but deferred
to a later implementation milestone.

### Error types

`IndexError` and `KeyError` are stub error types. Full error handling semantics
are defined in a separate PDP. The `get(key, default)` overload always returns
`T` directly and never throws.

## Type Rules

- Element types must be uniform — mixed-type literals are a compile-time error
- `K` in `Map[K, V]` and `T` in `Set[T]` must be hashable — enforced at compile time
- Mutating methods require a `var` receiver — enforced at compile time
- Empty literals without annotation are a compile-time error

## Out of Scope

- User-defined generics — `List`, `Map`, `Set` are built-in special forms;
  `[T]` parameter syntax is not yet available to user-defined types
- `OrderedMap[K, V]`, `SortedMap[K, V]` — stdlib types, available post-generics
- Fixed-size `Array[T, N]` — separate PDP if ever
- Slices and range syntax (e.g. `xs[1..3]`) — separate PDP
- Iteration syntax (`for x in xs`) — separate PDP
- Full error handling semantics — separate PDP
- Constructing one collection type from another (e.g. `Set[T]` from `List[T]`)

## Open Questions

- Does `get` overloading (`get(key)` vs `get(key, default)`) require Pipe to
  support function overloading? If overloading is not available at implementation
  time, `get_or(key, default)` is the fallback name.
- Should the compiler warn when a struct defines `hash()` but not `eql()`,
  or vice versa?
