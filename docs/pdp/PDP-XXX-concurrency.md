# PDP-XXX: Concurrency Model

**Status:** Draft (ongoing design discussion)
**Issues:** #

## Summary

Pipe's concurrency model supports both I/O-bound and CPU-bound parallelism. The caller
decides the execution context — no `async`/`suspend` marker on functions. The model rests
on three rules: structured task lifetimes, no mutable sharing between tasks, and an
explicit escape hatch (`atom`) for genuinely shared mutable state.

## Syntax

### `spawn` — concurrent task, returns a future

```pipe
const handle = spawn compute(x, y)
const result = try handle.get()
```

`spawn expr` runs `expr` concurrently and returns `Future[E!T]`. The future binding must
be consumed before scope exit (linearity-lite). A cancelled task resolves with the
built-in `CancelledError`, catchable normally:

```pipe
const result = handle.get() catch CancelledError { 0 }
```

### `scope` — explicit structured boundary

```pipe
scope {
    const a = spawn phase_one()
    const b = spawn phase_two()
} // both complete (or one errors) before execution continues
```

### `task_group` — dynamic task management

```pipe
const group = task_group()

const a = group.spawn(work_a())
const b = group.spawn(work_b())
group.cancel(a)
const result = try b.get()
// scope exit: group waits for any remaining tasks
```

### `race` — first result wins, others cancelled

```pipe
const result = race {
    spawn http_get(url)
    spawn sleep(5s) => HttpError.Timeout
}
```

### `channel` — streaming communication

```pipe
const ch = channel()      // unbuffered (rendezvous)
const ch = channel(64)    // buffered

spawn producer(ch)
for item in ch { process(item) }   // terminates when ch is closed
```

### `atom` — explicit shared mutable state

```pipe
const counter = atom(0)

counter.get()
counter.set(42)
counter.update(n => n + 1)    // atomic read-modify-write, retries on conflict
```

## Semantics

### Execution model

M:N green thread scheduler: lightweight tasks multiplexed onto OS threads by the runtime.
Blocking I/O parks the task without blocking the OS thread.

Cancellation is cooperative, checked automatically at suspension points (channel ops, I/O,
sleep). CPU-bound tasks need an explicit cancellation point — see Open Questions.

### Structured lifetime

The function body is an implicit scope — tasks spawned inside complete before the function
returns. Explicit `scope { }` subdivides a function into phases. Tasks cannot outlive
their scope: when a scope exits, it waits for all owned tasks, propagates any error up,
and cancels remaining children if one errored.

### `task_group` ownership

`task_group` is a plain value — passable, returnable, storable in data structures.
Returning it transfers the linearity-lite obligation to the caller's scope. The caller
must join or cancel before their scope exits.

### `race`

Sugar for `task_group` with a first-success policy. Natural fit for timeouts and hedged
requests.

### `channel` ownership

Channels carry `const` values — immutable messages. Sending a `var` value moves it into
the channel: the sender loses access, the receiver takes ownership.

### `atom`

`update` must receive a pure function — no side effects — because the runtime retries on
conflict (compare-and-swap semantics). For coordinated changes across multiple values,
prefer a single owner task over multiple atoms:

```pipe
// prefer: one task owns the state, others send messages
spawn account_manager(a: 1000, b: 500)

// or: group related mutable state into one atom
const accounts = atom({a: 1000, b: 500})
accounts.update(s => {a: s.a - 100, b: s.b + 100})
```

### Data sharing rules

| Value | Across task boundary | Cost |
|---|---|---|
| `const` | freely shareable by reference | zero |
| `var` | must be moved — binding consumed at spawn/send site | linearity-lite check |
| `Atom[T]` | shareable — that is its purpose | atomic ops |
| `Future[T]` | must be consumed before scope exit | linearity-lite check |
| `TaskGroup[T]` | must be joined or cancelled before scope exit | linearity-lite check |

### Error handling (PDP-005 integration)

`spawn f()` where `f` returns `E!T` produces `Future[E!T]`. `try handle.get()`
propagates the error and cancels scope siblings. `handle.get() catch e { ... }` handles
locally — siblings unaffected. `CancelledError` is a built-in error variant caught with
normal `catch`. Linearity-lite applies to `Future[T]` and `TaskGroup[T]` exactly as it
does to `E!T`.

## Design Decisions

- **No color on functions.** Concurrency is expressed by the caller via `spawn`, not
  declared by the callee. Functions are unaware of their execution context. Resolves the
  "What colour is your function" problem (Bob Nystrom).
- **Structured by default.** Tasks cannot outlive their scope. Errors propagate up and
  cancellation cascades down automatically — no task leaks.
- **No mutable sharing.** Race conditions are impossible by construction. `const` covers
  shareable data; `var` move covers handoff; `atom` covers explicit shared mutable state.
  No locks, no actors, no memory model to reason about.
- **`atom` not STM.** Full software transactional memory is powerful but complex. `atom`
  with pure `update` plus the single-owner task pattern covers the vast majority of cases.
- **Linearity-lite as a unified mechanism.** `Future[T]`, `TaskGroup[T]`, and moved `var`
  bindings follow the same consume-before-scope-exit rule already established by PDP-005.
- **Two-tier lifetime.** Structured scopes are the safe default. Returning a `TaskGroup`
  or `Future` to the caller is the explicit escape hatch — ownership transfers with the
  value, no additional syntax needed.

## Prior Art

| Language | Influence |
|---|---|
| Go | no color, goroutine spawn model, CSP channels |
| Kotlin | structured concurrency, cooperative cancellation |
| Swift | two-tier structured/detached, `TaskGroup` |
| Java Loom | `StructuredTaskScope`, shutdown policies |
| Erlang/Elixir | no mutable sharing philosophy |
| Clojure | `atom` with pure `update`, explicit shared mutable declaration |
| Haskell `async` | `concurrently`, `race` — futures without color |

## Out of Scope

- `|>` pipeline parallelism — builds on this model, separate PDP
- Supervision and restart trees
- STM (software transactional memory)
- Scheduler configuration
- `defer` / `errdefer` for resource cleanup on cancellation — separate PDP

## Open Questions

- **Channel close semantics** — who closes a channel and is it enforced? What happens on
  send to a closed channel — error or panic?
- **Backpressure** — when a buffered channel is full, does the sender block, drop, or get
  an error?
- **`select`** — waiting on whichever of N channels has data first. `race` covers
  competing tasks; `select` covers competing channel operations. Needed for "process work
  OR respond to shutdown signal."
- **CPU-bound cancellation** — long compute loops have no automatic cancellation point.
  Should this be `try check_cancel()` (fits PDP-005 naturally), a loop annotation, or
  convention only?
- **Resource cleanup on cancellation** — a cancelled task may hold open resources. Needs
  `defer` (always runs on scope exit) or `errdefer` (runs on error/cancel). Depends on a
  separate `defer` PDP.
- **Future consumers** — can two tasks both call `.get()` on the same future (broadcast),
  or is it single-consumer only?
- **`atom` purity enforcement** — `update` must receive a pure function. Compile-time,
  runtime warning, or convention?
