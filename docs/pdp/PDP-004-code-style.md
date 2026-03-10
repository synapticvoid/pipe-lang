# PDP-004: Code Style Conventions

**Status:** Draft
**Issues:** #

## Summary

Defines the canonical naming and casing conventions for Pipe and its standard
library. All stdlib APIs must follow these rules, and user code is expected to
do the same.

## Casing Rules

| Symbol kind | Convention | Example |
|---|---|---|
| Types, structs, enum variants | `PascalCase` | `Int`, `List[T]`, `Option.Some` |
| Variables, fields, parameters | `snake_case` | `user_name`, `max_retries` |
| Functions, methods | `snake_case` | `to_int()`, `is_empty()` |
| Module-level constants | `SCREAMING_SNAKE` | `MAX_SIZE`, `DEFAULT_TIMEOUT` |
| Type parameters | Single uppercase letter | `T`, `K`, `V` |
| Files, modules | `snake_case` | `http_client.pipe` |

## Acronyms

Acronyms are treated as regular words — only the first letter is capitalised,
following the rule of the symbol they appear in:

- `HttpClient` not `HTTPClient`
- `JsonParser` not `JSONParser`
- `parse_url()` not `parse_URL()`
- `to_json()` not `to_JSON()`

No exceptions.

## Naming Philosophy

**Shortest unambiguous full word.** No abbreviations, no padding:
- `size` not `length` or `len`
- `contains` not `has` or `incl`
- `remove` not `del` or `rm`

No synonyms in stdlib — one canonical name per concept.

## Standard Vocabulary

### Collections

| Operation | `List[T]` | `Set[T]` | `Map[K, V]` |
|---|---|---|---|
| Add (end / unordered) | `add(x)` | `add(x)` | `map[k] = v` |
| Add at index | `add(i, x)` | — | — |
| Remove by value / index / key | `remove(x)` / `remove(i)` | `remove(x)` | `remove(k)` |
| Membership | `contains(x)` | `contains(x)` | `contains(k)` |
| Size | `size` | `size` | `size` |
| Emptiness | `is_empty()` | `is_empty()` | `is_empty()` |
| First / last element | `first` / `last` | — | — |

### Type Conversion

Conversion methods are named `to_<type>()` and live on the source value:

```
"3".to_int()      # ParseError!Int
"3.14".to_float() # ParseError!Float
42.to_str()       # Str
```

Fallible conversions return an error union and must be handled explicitly:

```
var n = try "42".to_int()
var n = "abc".to_int() catch 0
```

### String Operations

| Operation | Name |
|---|---|
| Split | `split(separator)` |
| Trim whitespace | `trim()` |
| Trim specific chars | `trim(chars)` |
| Replace | `replace(from, to)` |
| Starts / ends with | `starts_with(s)` / `ends_with(s)` |
| Uppercase / lowercase | `to_upper()` / `to_lower()` |

### String Representation

Any struct can override `to_str()` to control its string representation.
This is the canonical way to make a type printable. The protocol backing
this is deferred to a future PDP.

## Rationale

- `snake_case` for functions and variables matches the scripting world (Python,
  Ruby, Rust) and reads well for long identifiers.
- `PascalCase` for types and enum variants gives a consistent visual signal for
  type-level constructs, including enum variants used as constructors
  (`Result.Ok(x)`).
- `SCREAMING_SNAKE` for constants follows near-universal convention.
- Single-letter type parameters (`T`, `K`, `V`) are idiomatic and universally
  understood — full names add noise in generic signatures.
- Treating acronyms as words eliminates a whole class of inconsistency and
  keeps all casing rules exceptionless.

## Open Questions

- None.
