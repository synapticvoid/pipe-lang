# Pipe Lang

Toy language implementation, ported from Python to Zig 0.15.2 as a learning exercise.

## Project structure

- `src/tokens.zig` — Token types and keyword lookup
- `src/lexer.zig` — Lexer/tokenizer
- `src/main.zig` — Entry point

## Workflow

- Build: `make build` (or `zig build`)
- Run: `zig build run`
- Test: `make test` (or `zig build test`)
- Install to `/usr/local/bin/`: `make install`
- Show targets: `make help`

## Conventions

- Guide the user step by step — provide structure/skeleton first, let them implement
- Don't give full implementations; explain Zig-specific concepts at each step
- Review their code when asked ("check", "critize") — focus on correctness and Zig idioms
- Keep explanations concise and focused on what differs from Python
