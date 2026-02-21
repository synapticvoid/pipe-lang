Using zig 0.15.2.

## Syntax
```zig
// All types are in PascalCase
// Int, Float, Bool, String, etc
// Variable declaration
var a: Int = 1;

// Type inference
var b = 2.5;

// constant values
const name = "Bob";

// Function declaration
fn add(a: Int, b: Int) Int {
    return a + b;
}

// Control flow - if
if a > b {
    print("a is greater than b");
}

// if is an expression
var max = if a > b { a } else { b };
print("The max is $max);

// Control flow - when
when a {
    1 => print("a is 1");
    2 => print("a is 2");
    else => print("a is not 1 or 2");
}

// Error handling - declare an error type
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

// Easy path: untyped error with a message
fn lookup(id: Int) !String {
    return fail("not found");
}
```


