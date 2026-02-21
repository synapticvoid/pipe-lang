# Static Typing Plan

Pipeline: **Source → Lexer → Parser → AST → `Type Checker` → Interpreter**

## Step 1 — Define a type system (`src/types.zig`)
- [x] Create `PipeType` enum: `int`, `float`, `bool`, `string`, `unit`
- [ ] Function type carries param types + return type (deferred to Step 4)

## Step 2 — Add `const` support
- [x] Add `const` keyword/token (⚠️ add to keyword map too!)
- [x] Add `mutability: Mutability` to `VarDeclaration` AST node
- [x] Parse `const name = ...;` alongside `var name = ...;`
- [ ] Reject reassignment of `const` bindings (deferred to Step 4)

## Step 3 — Add optional type annotations
- [x] Add `colon` token to lexer
- [x] Add `type_annotation: ?Token` to `VarDeclaration`
- [x] Parse `var a: Int = 1;` syntax
- [x] Extend `FnDeclaration` params to carry types + return type annotation (null = Unit)

## Step 4 — Build the type checker (`src/type_checker.zig`)
- [x] Create `TypeEnvironment` (maps variable names → `PipeType`)
- [x] Infer types from literals
- [x] Check binary/unary operator types
- [x] Var declarations: validate annotation vs initializer, or infer
- [x] Assignments: check value matches variable's type
- [x] Const enforcement: reject reassignment
- [x] If expressions: both branches must produce the same type
- [x] Functions: type-check body, validate return type (return type validation TODO)

## Step 4b — Function calls & `Any` type
- [x] Add `any` variant to `PipeType`
- [x] Add `compatible(other)` method to `PipeType` (returns true if either is `any` or both are equal)
- [x] Replace `==` type comparisons with `compatible()` in type checker
- [x] Store function signatures in `TypeEnvironment` (param types + return type)
- [x] Register built-in functions (`print(Any) → Unit`) in root environment
- [x] `checkFnCall`: validate arg count, check each arg type against param type
- [x] `checkFnCall`: return the function's declared return type
- [x] Validate return type in `checkFunctionDeclaration`

## Step 5 — Wire into `main.zig`
- [x] Run type checker between parser and interpreter
- [x] Type errors stop execution before interpretation

## Step 6 — Improve error reporting
- [ ] Include line numbers in type error messages
- [ ] Clear messages: "Type mismatch: expected Int, got String at line 5"
