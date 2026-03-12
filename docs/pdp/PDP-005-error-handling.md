# PDP-005: Error Handling

**Status:** Implemented
**Issues:** #

## Summary

Errors are values in Pipe — no `throw`/`raise` exists. This PDP defines
`error enum` declarations, `E!T` error union types, and the `try`/`catch`
operators.

## Syntax & Semantics

### Error enums

```pipe
error enum MathError { DivByZero, Overflow }

# Variants with fields
error enum ParseError {
    InvalidChar(const ch: Str),
    UnexpectedEof,
}
```

Like a regular `enum` but valid in the error position of `E!T`. An
`error enum` can nest another to form a hierarchy — coercion rules from
the enum system apply:

```pipe
error enum AppError { MathError, IoError }
```

### Error union types (`E!T`)

`E!T` means "either `T` or an error of type `E`" (`E` must be an
`error enum`). Internally the type checker synthesizes
`enum { Ok(value: T), Err(err: E) }` — this is not user-visible.

```pipe
fn divide(a: Int, b: Int) MathError!Int {
    if b == 0 { return MathError.DivByZero(); }
    a / b   # plain T is implicitly wrapped in Ok
}
```

### `try` — propagate errors

`try expr` unwraps `Ok(v) → v` or returns `Err(e)` from the enclosing
function. Compile-time error if `expr` is non-fallible or the enclosing
function isn't `E!T`.

```pipe
fn compute(a: Int, b: Int) MathError!Int {
    const x = try divide(a, b);
    x + 1
}
```

### `catch` — handle errors

`catch` unwraps `Ok(v) → v` or runs the handler on `Err`. Handler return
type must match `T`. Compile-time error if the expression is non-fallible.

```pipe
divide(10, 0) catch e { log(e); -1; };   # block form
divide(10, 0) catch e => -1;              # one-liner
divide(10, 0) catch { -1; };              # without binding
```

### Explicit discard

```pipe
const _ = fallible_call();
```

### Unconsumed fallible tracking

Every `E!T` binding must be consumed before scope exit. A binding is
consumed when it is: passed to `try` or `catch`, returned, passed to a
function expecting `E!T`, rebound to another variable, or assigned to `_`.

Violations (all compile-time errors):
- Discarding a fallible result without binding
- Exiting scope with an unconsumed fallible binding
- Returning a non-fallible value while fallible bindings remain
- Reassigning a `var` holding an unconsumed fallible

Both branches of `if`/`else` must consume the same fallible bindings.
`if` without `else` must not consume any.

```pipe
if cond { try result; } else { try result; }  # valid
if cond { try result; } else { 0; }           # invalid
if cond { try result; }                        # invalid
```

## Design Decisions

- **Errors are values.** Return an error variant to produce an error —
  same syntax as value creation, flow visible in the return type.
- **Linearity-lite.** Fallible bindings must be consumed (like Rust's
  `#[must_use]` but per-binding). Prevents silently ignored errors.
- **`try` is an expression.** `try expr` evaluates to `T`, usable inline.
- **No inferred error unions yet.** `!T` is parsed but not implemented;
  all error unions must name `E` explicitly.

## Out of Scope

- Inferred error unions (`!T`)
- `when` matching on error variants (pattern matching PDP)
- Error set union in pipe chains (PDP-010)
- Stack traces / error context
- `errdefer` / cleanup-on-error
- `panic` / `assert` (separate PDP)

## Open Questions

- Should `!T` (inferred error union) be implemented?
