# PDP-015: Null Safety

**Status:** Draft
**Issues:** #32

## Summary

Add `T?` as a built-in first-class optional type — the same relationship `E!T` has to `Result[E, T]`. The `null` literal is only valid where a `T?` type is expected, `T` widens to `T?` automatically, and unwrapping is explicit via `orelse` or `when` pattern matching.

## Syntax

```
TODO
```

## Semantics

TODO

## Open Questions

- TODO
