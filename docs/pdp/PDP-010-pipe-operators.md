# PDP-010: Pipe Operators

**Status:** Accepted
**Issues:** #22

## Summary

Add `|>` and `!>` pipeline operators for left-to-right value threading. `|>`
forwards plain values, `!>` binds over fallible values and short-circuits on
error. These operators are the foundation for shell call chaining (PDP-011).

## Syntax

```pipe
# plain pipe — no extra args
const result = input |> trim |> upper;

# plain pipe — extra args, `in` marks insertion point
const parts = input |> trim |> split(in, " ");

# fallible pipe
const saved = input !> validate(in) !> save(in, path) catch e => recover(e);

# mixed: plain into fallible
const result = input |> parse |> trim(in) !> validate(in) !> save(in, path);

# in at any argument position
const replaced = input |> replace(old, in, new);

# with implicit lambda param
const names = users |> filter(in, fn => it.active) |> map(in, fn => it.name);
```

## Semantics

### `|>` — plain pipe

`x |> f` desugars to `f(x)`. LHS must be `T` (non-fallible).

When the RHS has additional arguments, `in` marks the insertion point:
- `x |> f(in, a)` → `f(x, a)`
- `x |> f(a, in)` → `f(a, x)`

`in` can appear at any argument position.

### `!>` — fallible pipe

`r !> f` where `r: E!T`:
- `Ok(v)` → call `f(v)`, propagate result
- `Err(e)` → short-circuit, skip RHS, propagate error

LHS must be `E!T`. The unwrapped `T` is forwarded. Even if the RHS function is
non-fallible (`T -> U`), the result is `E!U` — the upstream error still
propagates.

Error types compose across stages: `E1!A !> (A -> E2!B)` yields `(E1|E2)!B`.
The compiler synthesizes the union — no annotation required.

### `in` — pipe placeholder

`in` is a reserved keyword that refers to the value flowing into the current
pipe stage. It is only valid on the RHS of `|>` or `!>`. Required when the
RHS call has additional arguments; omitted otherwise (`|> f` not `|> f(in)`).
Exactly one `in` per stage. `|> f()` is a parse error — use `|> f` or
`|> f(in, ...)`.

### `catch`

Applies to the whole preceding pipeline expression using the existing form:

```pipe
input !> validate(in) !> save(in, path) catch e => recover(e)
```

Without `catch`, a chain containing `!>` stays fallible and must be handled at
the call site. `catch` on a pure `|>` chain is a type error — no fallible value
to catch.

### Associativity and precedence

Left-associative: `a |> f !> g(in)` parses as `((a |> f) !> g(in))`.

Lower precedence than arithmetic and comparison operators:
- `a + b |> f` → `(a + b) |> f`
- `x |> f == y` → `(x |> f) == y`

## Type Rules

- `|>` requires plain LHS (`T`). Using `|>` on `E!T` is a type error — the
  compiler suggests `!>`.
- `!>` requires fallible LHS (`E!T`). Using `!>` on `T` is a type error — the
  compiler suggests `|>`.
- Without `catch`, a chain with `!>` stays fallible.
- With `catch`, result type is the handler return type.

## `in` and `it`

`in` is the pipe placeholder — the value flowing into a stage. `it` is the
implicit single-parameter name inside lambdas. They are distinct and coexist
without ambiguity:

```pipe
users |> map(in, fn => it.name)  // `in` = users list, `it` = each user
```

## Out of Scope

- Pipeline into non-call RHS
- Multiple `in` per stage
- Async/stream pipelines
- Explicit error union syntax beyond inferred pipeline results

## Open Questions

- Should multiple `in` per stage be a parse error or a type error?
- Does `in` conflict with future `for x in list` syntax? Likely fine since
  the pipe RHS context is unambiguous, but worth revisiting when iteration
  is designed.
