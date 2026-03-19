# PDP-010: Pipe Operator

**Status:** Accepted
**Issues:** #22

## Summary

Add a `|>` pipeline operator for left-to-right value threading. `|>` forwards
values through function chains. When the LHS is fallible (`E!T`), the operator
auto-unwraps on `Ok` and short-circuits on `Err`. Error types compose
automatically across stages. This operator is the foundation for shell call
chaining (PDP-011).

## Syntax

```pipe
# plain pipe — no extra args
const result = input |> trim |> upper;

# plain pipe — extra args, `in` marks insertion point
const parts = input |> trim |> split(in, " ");

# fallible pipe — type checker handles unwrapping/short-circuit
const saved = input |> validate(in) |> save(in, path) catch e => recover(e);

# mixed: plain into fallible
const result = input |> parse |> trim(in) |> validate(in) |> save(in, path);

# in at any argument position
const replaced = input |> replace(old, in, new);

# with implicit lambda param
const names = users |> filter(in, fn => it.active) |> map(in, fn => it.name);
```

## Semantics

### `|>` — pipe

When LHS is plain `T`: `x |> f` desugars to `f(x)`.

When LHS is fallible `E!T`:
- `Ok(v)` → call `f(v)`, propagate result
- `Err(e)` → short-circuit, skip RHS, propagate error

Even if the RHS function is non-fallible (`T -> U`), the result is `E!U` — the
upstream error still propagates.

Error types compose across stages: `E1!A |> (A -> E2!B)` yields `(E1|E2)!B`.
The compiler synthesizes the union — no annotation required.

When the RHS has additional arguments, `in` marks the insertion point:
- `x |> f(in, a)` → `f(x, a)`
- `x |> f(a, in)` → `f(a, x)`

`in` can appear at any argument position.

### `in` — pipe placeholder

`in` is a reserved keyword that refers to the value flowing into the current
pipe stage. It is only valid on the RHS of `|>`. Required when the RHS call has
additional arguments; omitted otherwise (`|> f` not `|> f(in)`). Exactly one
`in` per stage. `|> f()` is a parse error — use `|> f` or `|> f(in, ...)`.

### `catch`

Applies to the whole preceding pipeline expression using the existing form:

```pipe
input |> validate(in) |> save(in, path) catch e => recover(e)
```

Without `catch`, a chain containing fallible stages stays fallible and must be
handled at the call site. `catch` on a pure non-fallible chain is a type
error — no fallible value to catch.

### Associativity and precedence

Left-associative: `a |> f |> g(in)` parses as `((a |> f) |> g(in))`.

Lower precedence than arithmetic, higher than comparison:
- `a + b |> f` → `(a + b) |> f`
- `x |> f == y` → `(x |> f) == y`

`catch` has lower precedence than `|>`, so it applies to the whole chain:
- `a |> b |> c catch e => d` → `(a |> b |> c) catch e => d`
- Use parens to catch midway: `(input |> validate(in) catch e => default) |> save(in, path)`

## Type Rules

- When LHS is `T` and RHS returns `U`, result is `U`.
- When LHS is `T` and RHS returns `E!U`, result is `E!U` (chain becomes
  fallible).
- When LHS is `E!T` and RHS returns `U`, result is `E!U` (upstream error
  propagates).
- When LHS is `E1!T` and RHS returns `E2!U`, result is `(E1|E2)!U` (errors
  compose).
- Without `catch`, a chain with any fallible stage stays fallible.
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

## Resolved Questions

- **Does `in` conflict with `for x in list`?** No. `in` is only a pipe
  placeholder on the RHS of `|>`. In a `for` header, the parser recognizes
  `in` as the iteration keyword from context. No ambiguity.
