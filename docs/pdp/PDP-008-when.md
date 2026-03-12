# PDP-008: When Expression

**Status:** Draft
**Issues:** #15

## Summary

`when` is an exhaustive pattern-matching expression. It works on any type — unions, errors, literals — using `pattern => expression` arm syntax. All arms must return the same type when used as an expression, and the type checker enforces that every variant is covered or a `_` wildcard is present.

## Syntax

```
TODO
```

## Semantics

TODO

## Open Questions

- TODO
