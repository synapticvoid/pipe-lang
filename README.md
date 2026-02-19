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
```


