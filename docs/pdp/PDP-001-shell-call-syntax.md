# PDP-001: Shell Call Syntax

**Status:** Draft

## Summary

Add first-class shell interop with safe defaults. `^...` builds structured
command values. Execution is explicit and lazy — pipelines are data until
`shell.run()` fires them. The `!>` operator threads errors through the chain
naturally.

## Syntax

```pipe
# single command
shell.cmd(^git push)
    !> shell.run()
    catch e { log(e.code) }

# pipeline
shell.cmd(^ps aux)
    !> shell.cmd(^grep $pattern)
    !> shell.cmd(^wc -l)
    !> shell.run()
    catch e {
        when e {
            CommandError.NonZero  -> log(e.code)
            CommandError.Spawn    -> log("binary not found")
        }
    }

# shell escape hatch
shell.shell("ps aux | grep #{pattern} | sort")
    !> shell.run()
    catch e { ... }

# propagating
fn deploy() CommandError!RunResult {
    try shell.cmd(^git push) !> shell.run()
}
```

## Semantics

### `^...` — CommandSpec literal

Builds a `CommandSpec` — structured command data, not a string. First token
is the program, rest are arguments. `$name` and `#{expr}` each inject exactly
one argument. No shell expansion: no globbing, no redirects, no substitution.

### `shell.cmd(spec)`

Returns `CommandError!Pipeline`. Lazy — no execution. When receiving a
`Pipeline` via `!>`, appends the command to it. First call in a chain creates
a new `Pipeline`.

### `shell.shell(str)`

Returns `CommandError!Pipeline`. Lazy. Wraps a raw shell string for execution
via the system shell. String interpolation with `#{}` is supported. Explicit
escape hatch — shell expansion applies.

### `shell.run()`

Receives a `Pipeline` via `!>`, executes it as a proper OS-level pipeline
(processes connected by pipes, not sequential). Returns `CommandError!RunResult`.

## Types

```pipe
case struct CommandSpec(program: String, args: List<String>);
case struct RunResult(code: Int, stdout: String, stderr: String);

error enum CommandError {
    NonZero(const code: Int),
    Spawn,
}
```

`stdout` and `stderr` are fully buffered strings. Streaming output is deferred
— it requires an iterator/stream type and depends on generics (#27).

## Open Questions

- Is `Spawn` enough or do we need more variants (e.g. `Timeout`)?
- Namespace: `shell.*` — final name TBD
