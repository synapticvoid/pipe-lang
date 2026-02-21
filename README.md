![CI](https://github.com/synapticvoid/pipe-lang/actions/workflows/ci.yml/badge.svg)
![Zig](https://img.shields.io/badge/zig-0.15.2-f7a41d)
![License](https://img.shields.io/github/license/synapticvoid/pipe-lang)

## Syntax

### Variables
```zig
// All types are in PascalCase: Int, Float, Bool, String, etc
var a: Int = 1   // explicit type
var b = 2.5      // type inference
const name = "Bob"
```

### Functions
```zig
fn add(a: Int, b: Int) Int {
    return a + b;
}
```

### Control flow
```zig
// if statement
if a > b {
    print("a is greater than b");
}

// if as an expression
var max = if a > b { a } else { b };
print("The max is $max);

// when (pattern matching)
when a {
    1 => print("a is 1");
    2 => print("a is 2");
    else => print("a is not 1 or 2");
}
```

### Error handling
```zig
// Declare an error type
error MathError { DivByZero, Overflow }

// Fallible function: returns MathError!Int or !Int (inferred)
fn divide(a: Int, b: Int) MathError!Int {
    if b == 0 { return MathError.DivByZero }
    return a / b;
}

// try propagates the error up (caller must also be fallible)
fn safeDivide(a: Int, b: Int) !Int {
    return try divide(a, b);
}

// catch handles it inline
const result = divide(10, 0) catch { -1 };

// catch with binding
const result = divide(10, 0) catch |e| { print(e); -1 };

// Untyped error with a message
fn lookup(id: Int) !String {
    return fail("not found");
}
```


