# PDP-003: Functions

**Status:** Partially Implemented
**Issues:** #

## Summary

Define function declarations, calls, closures, and first-class function values.
Functions are values in Pipe — they can be stored in variables and passed as
arguments.

## Syntax

### Declaration

```pipe
fn greet(name: Str) Str {
    return "Hello, " + name;
}

# Unit return type (implicit)
fn log(msg: Str) {
    print(msg);
}
```

Parameters require type annotations. Return type is optional — defaults to
`Unit` when omitted.

### Calls

```pipe
greet("Alice");
const result = add(1, 2);
```

### Closures

Functions capture their enclosing environment:

```pipe
fn make_adder(n: Int) Fn(Int) Int {
    fn add(x: Int) Int {
        return n + x;
    }
    return add;
}

const add5 = make_adder(5);
add5(3);   # 8
```

### Function types

Function type annotations use `Fn(params) return_type`:

```pipe
# A function that takes an Int and returns an Int
const transform: Fn(Int) Int = double;

# A function that takes two Strs and returns a Bool
fn apply(predicate: Fn(Str, Str) Bool, a: Str, b: Str) Bool {
    return predicate(a, b);
}

# Higher-order: accepting and returning functions
fn compose(f: Fn(Int) Int, g: Fn(Int) Int) Fn(Int) Int {
    fn composed(x: Int) Int {
        return f(g(x));
    }
    return composed;
}
```

`Fn(...)` mirrors the `fn` keyword — `Fn` for types, `fn` for declarations.

### Implicit last-expression return

The last expression in a function body is its return value. Explicit `return`
is also supported:

```pipe
fn double(x: Int) Int {
    x * 2
}

fn double_explicit(x: Int) Int {
    return x * 2;
}
```

## Semantics

### Parameters

All parameters are passed by value. Parameter type annotations are mandatory.
Calling a function with the wrong number of arguments or incompatible types is
a compile-time error.

### Return type

When a return type is declared, the type checker verifies that the body's type
(last expression or explicit `return`) is compatible with it. A mismatch is a
compile-time error.

When no return type is declared, the function returns `Unit`.

### Closures

A function captures a reference to its enclosing environment at declaration
time. Captured variables are live — mutations to a `var` binding in the outer
scope are visible inside the closure, and vice versa.

### Recursion

A function's name is registered in the enclosing scope before its body is
type-checked, so direct recursion works without forward declarations:

```pipe
fn factorial(n: Int) Int {
    if n <= 1 { 1 }
    else { n * factorial(n - 1) }
}
```

### Functions as values

Functions are first-class values. They can be stored in `const`/`var` bindings,
passed as arguments, and returned from other functions. The `Fn(params) return`
type annotation describes the signature.

### Fallible functions

Functions that return an error union declare it in the return type:

```pipe
fn divide(a: Int, b: Int) MathError!Int {
    if b == 0 { return MathError.DivByZero; }
    return a / b;
}
```

Full error handling semantics (try/catch, fallible tracking) are defined in the
error handling PDP.

## Type Rules

- Parameter annotations are mandatory — no implicit `Any`
- Return type is optional — defaults to `Unit`
- Body type must be compatible with declared return type
- Argument count and types must match parameter list at call site
- `Fn(A, B) R` is compatible with another `Fn(A, B) R` when parameter and
  return types match

## Out of Scope

- Anonymous functions / lambdas (`fn => ...` / `fn(x) => x * 2`)
- Variadic functions
- Default parameter values
- Named arguments
- Overloaded functions
- Methods and `Self` (struct PDP)

## Design Decisions

- `Fn` type syntax supports fallible return types: `Fn(Int) MathError!Int`.

## Open Questions

- Should closures support `const` capture (snapshot) in addition to reference
  capture?
