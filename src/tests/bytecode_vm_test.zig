const std = @import("std");
const pipe = @import("pipe");
const vm_pkg = pipe.vm;
const Chunk = vm_pkg.Chunk;
const OpCode = vm_pkg.OpCode;
const Vm = vm_pkg.Vm;
const Program = vm_pkg.Program;
const Value = vm_pkg.Value;
const StructDef = vm_pkg.StructDef;
const StructKind = pipe.ast.StructKind;
const Compiler = vm_pkg.Compiler;
const ast = pipe.ast;
const Token = pipe.Token;
const RuntimeContext = pipe.RuntimeContext;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a chunk, run it, return the last value on the stack (from return).
/// Takes ownership of the chunk — caller must NOT deinit it.
fn runChunk(chunk: *Chunk) !Value {
    var program = Program.init(std.testing.allocator);
    defer program.deinit();
    program.chunk = chunk.*;
    chunk.* = Chunk.init(std.testing.allocator);
    var aw: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer aw.deinit();
    const ctx = RuntimeContext{ .writer = &aw.writer };
    var vm = Vm.init(&program, ctx, std.testing.allocator);
    defer vm.deinit();
    return try vm.run();
}

/// Run a chunk with pre-registered struct definitions.
/// Takes ownership of the chunk — caller must NOT deinit it.
fn runChunkWithStructDefs(chunk: *Chunk, defs: []const StructDef) !Value {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var program = Program.init(allocator);
    defer program.deinit();

    for (defs) |def| {
        _ = try program.addStructDef(def);
    }

    program.chunk = chunk.*;
    chunk.* = Chunk.init(std.testing.allocator);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const ctx = RuntimeContext{ .writer = &aw.writer };
    var vm = Vm.init(&program, ctx, allocator);
    defer vm.deinit();
    return try vm.run();
}

/// Capture print output during VM execution.
/// Takes ownership of the chunk — caller must NOT deinit it.
fn runChunkCapturingOutput(chunk: *Chunk) !struct { result: Value, output: []const u8 } {
    var program = Program.init(std.testing.allocator);
    defer program.deinit();
    program.chunk = chunk.*;
    chunk.* = Chunk.init(std.testing.allocator);
    var buf = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer buf.deinit();
    const ctx = RuntimeContext{ .writer = &buf.writer };
    var vm = Vm.init(&program, ctx, std.testing.allocator);
    defer vm.deinit();

    const result = try vm.run();
    return .{ .result = result, .output = try buf.toOwnedSlice() };
}

// ===========================================================================
// Step 2: Constants, literals, stack ops
// ===========================================================================

test "constant pushes value onto stack" {
    // Program: push 42, return
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx = try chunk.findOrAddConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "true literal" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.true, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = true }));
}

test "false literal" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.false, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = false }));
}

test "null literal" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.null, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.null));
}

test "negate integer" {
    // Program: push 7, negate, return → -7
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx = try chunk.findOrAddConstant(.{ .int = 7 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx, 1);
    try chunk.writeOp(.negate, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = -7 }));
}

test "negate non-integer is type error" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.true, 1);
    try chunk.writeOp(.negate, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.TypeError, runChunk(&chunk));
}

test "not boolean" {
    // not true → false
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.true, 1);
    try chunk.writeOp(.not, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = false }));
}

test "not uses truthiness" {
    // not 0 → true (0 is falsy)
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx = try chunk.findOrAddConstant(.{ .int = 0 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx, 1);
    try chunk.writeOp(.not, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = true }));
}

test "pop discards top of stack" {
    // push 99, push 42, pop, return → 99
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const idx0 = try chunk.findOrAddConstant(.{ .int = 99 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx0, 1);

    const idx1 = try chunk.findOrAddConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx1, 1);

    try chunk.writeOp(.pop, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 99 }));
}

// ===========================================================================
// Step 3: Arithmetic, comparison, print, return
// ===========================================================================

test "add two integers" {
    // 10 + 32 → 42
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 10 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 32 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.add, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "subtract two integers" {
    // 50 - 8 → 42
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 50 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 8 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.subtract, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "multiply two integers" {
    // 6 * 7 → 42
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 6 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 7 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.multiply, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "divide two integers" {
    // 84 / 2 → 42
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 84 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 2 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.divide, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "divide by zero is error" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 0 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.divide, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.DivisionByZero, runChunk(&chunk));
}

test "equal integers" {
    // 42 == 42 → true
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.equal, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = true }));
}

test "not equal integers" {
    // 1 == 2 → false
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 2 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.equal, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = false }));
}

test "greater than" {
    // 5 > 3 → true
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 5 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 3 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.greater, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = true }));
}

test "less than" {
    // 3 < 5 → true
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.findOrAddConstant(.{ .int = 3 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.findOrAddConstant(.{ .int = 5 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.less, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = true }));
}

test "return with empty stack returns unit" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.unit));
}

test "complex expression: (2 + 3) * -4" {
    // push 2, push 3, add, push 4, negate, multiply, return → -20
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const c2 = try chunk.findOrAddConstant(.{ .int = 2 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c2, 1);

    const c3 = try chunk.findOrAddConstant(.{ .int = 3 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c3, 1);

    try chunk.writeOp(.add, 1);

    const c4 = try chunk.findOrAddConstant(.{ .int = 4 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c4, 1);

    try chunk.writeOp(.negate, 1);
    try chunk.writeOp(.multiply, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = -20 }));
}

// ===========================================================================
// Step 4: Variables and control flow
// ===========================================================================

test "define and get global variable" {
    // define_global "x" = 42, get_global "x", return → 42
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const name_idx = try chunk.findOrAddConstant(.{ .string = "x" });
    const val_idx = try chunk.findOrAddConstant(.{ .int = 42 });

    // Push value, then define_global
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val_idx, 1);
    try chunk.writeOp(.define_global, 1);
    try chunk.writeU16(name_idx, 1);

    // Get it back
    try chunk.writeOp(.get_global, 1);
    try chunk.writeU16(name_idx, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "set existing global variable" {
    // define_global "x" = 10, set_global "x" = 20, get_global "x", return → 20
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const name_idx = try chunk.findOrAddConstant(.{ .string = "x" });
    const val10 = try chunk.findOrAddConstant(.{ .int = 10 });
    const val20 = try chunk.findOrAddConstant(.{ .int = 20 });

    // Define x = 10
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val10, 1);
    try chunk.writeOp(.define_global, 1);
    try chunk.writeU16(name_idx, 1);

    // Set x = 20
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val20, 1);
    try chunk.writeOp(.set_global, 1);
    try chunk.writeU16(name_idx, 1);

    // Get x
    try chunk.writeOp(.get_global, 1);
    try chunk.writeU16(name_idx, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 20 }));
}

test "get undefined global is error" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const name_idx = try chunk.findOrAddConstant(.{ .string = "nope" });
    try chunk.writeOp(.get_global, 1);
    try chunk.writeU16(name_idx, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.UndefinedVariable, runChunk(&chunk));
}

test "set undefined global is error" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const name_idx = try chunk.findOrAddConstant(.{ .string = "nope" });
    const val = try chunk.findOrAddConstant(.{ .int = 1 });

    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val, 1);
    try chunk.writeOp(.set_global, 1);
    try chunk.writeU16(name_idx, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.UndefinedVariable, runChunk(&chunk));
}

test "get and set local variable" {
    // Locals live on the stack. Slot 0 is the "function" slot (convention).
    // For this MVP we just put locals directly on the stack.
    //
    // push null (slot 0 placeholder)
    // push 42   (slot 1 = local x)
    // get_local 1
    // return → 42
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.null, 1); // slot 0 placeholder
    const val = try chunk.findOrAddConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val, 1); // slot 1

    try chunk.writeOp(.get_local, 1);
    try chunk.writeU16(1, 1); // read slot 1

    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "set local variable" {
    // push null (slot 0), push 10 (slot 1)
    // push 99, set_local 1  ← overwrite slot 1
    // get_local 1, return → 99
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.null, 1); // slot 0
    const val10 = try chunk.findOrAddConstant(.{ .int = 10 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val10, 1); // slot 1

    const val99 = try chunk.findOrAddConstant(.{ .int = 99 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val99, 1);
    try chunk.writeOp(.set_local, 1);
    try chunk.writeU16(1, 1); // overwrite slot 1

    try chunk.writeOp(.get_local, 1);
    try chunk.writeU16(1, 1);

    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 99 }));
}

test "jump skips instructions" {
    // push 1, jump +6 (skip push 2 + return), push 3, return
    //
    // Layout:
    //   0: constant 1   (3 bytes)
    //   3: jump +6       (3 bytes) → jumps to offset 12
    //   6: constant 2   (3 bytes)  ← skipped
    //   9: return        (1 byte)  ← skipped
    //  10: constant 3   (3 bytes) ← wait, let me recalculate...
    //
    // Actually: jump operand = number of bytes to skip AFTER the jump instruction.
    // jump is 3 bytes (opcode + u16). After jump, ip is at offset 6.
    // We want to skip: constant(3) + return(1) = 4 bytes.
    // So jump offset = 4, landing at offset 10.
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const c1 = try chunk.findOrAddConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1); // offset 0
    try chunk.writeU16(c1, 1); // offset 1-2

    try chunk.writeOp(.jump, 1); // offset 3
    try chunk.writeU16(10, 1); // offset 4-5, jump to absolute offset 10

    const c_bad = try chunk.findOrAddConstant(.{ .int = 999 });
    try chunk.writeOp(.constant, 1); // offset 6 (skipped)
    try chunk.writeU16(c_bad, 1); // offset 7-8 (skipped)
    try chunk.writeOp(.@"return", 1); // offset 9 (skipped)

    const c3 = try chunk.findOrAddConstant(.{ .int = 3 });
    try chunk.writeOp(.constant, 1); // offset 10
    try chunk.writeU16(c3, 1);

    try chunk.writeOp(.add, 1); // 1 + 3
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 4 })); // 1 + 3
}

test "jump_if_false skips when falsy" {
    // push false, jump_if_false +4, push 999, return, push 42, return
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.false, 1); // offset 0

    try chunk.writeOp(.jump_if_false, 1); // offset 1
    try chunk.writeU16(8, 1); // offset 2-3, jump to absolute offset 8

    const c_bad = try chunk.findOrAddConstant(.{ .int = 999 });
    try chunk.writeOp(.constant, 1); // offset 4 (skipped)
    try chunk.writeU16(c_bad, 1); // offset 5-6 (skipped)
    try chunk.writeOp(.@"return", 1); // offset 7 (skipped)

    const c42 = try chunk.findOrAddConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1); // offset 8
    try chunk.writeU16(c42, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "jump_if_false falls through when truthy" {
    // push true, jump_if_false to offset 8, push 42, return, push 999, return
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.true, 1); // offset 0

    try chunk.writeOp(.jump_if_false, 1); // offset 1
    try chunk.writeU16(8, 1); // offset 2-3, jump to absolute offset 8 (not taken)

    const c42 = try chunk.findOrAddConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1); // offset 4
    try chunk.writeU16(c42, 1);
    try chunk.writeOp(.@"return", 1); // offset 7

    const c_bad = try chunk.findOrAddConstant(.{ .int = 999 });
    try chunk.writeOp(.constant, 1); // offset 8 (not reached)
    try chunk.writeU16(c_bad, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "loop jumps backward" {
    // Simple countdown: count from 3 to 0
    //
    //   push 3             ; slot 0 = counter
    // loop_start (offset 3):
    //   get_local 0        ; push counter
    //   push 0
    //   equal              ; counter == 0?
    //   jump_if_false +1   ; if not done, skip next return
    //   return             ; (skipped when not done, reached when counter == 0)
    //   (wait — this is getting complex, let's do something simpler)
    //
    // Simpler: loop that sums 1+2+3 using just decrement
    // Actually the simplest test: loop that runs exactly once backward
    //
    // offset 0: push 42
    // offset 3: jump +1     ; skip the loop opcode
    // offset 6: loop 4      ; jump back to offset 5... no this is tricky
    //
    // Let's just verify loop subtracts from ip:
    //
    // offset 0: push 42       (constant, 3 bytes)
    // offset 3: jump +3       (3 bytes) → skips to offset 9
    // offset 6: return        (1 byte)  ← target of loop
    // offset 7: (unreachable)
    // ...
    //
    // Simplest: just verify the backward jump mechanic.
    // A loop with a conditional exit:
    //
    // slot 0 = accumulator (starts at 0)
    // slot 1 = counter (starts at 3)
    //
    // 0:  constant 0     ; acc = 0 (slot 0)
    // 3:  constant 3     ; counter = 3 (slot 1)
    //
    // loop_top (offset 6):
    // 6:  get_local 1    ; push counter
    // 9:  constant 0     ; push 0
    // 12: equal          ; counter == 0?
    // 13: jump_if_false +1 ; if false (counter != 0), skip return
    // 16: get_local 0     ; push acc
    // 19: return
    //
    // wait — jump_if_false pops, so we need to be careful
    // Let me just test: loop that runs 0 iterations (already done)
    // and verify the mechanic with one jump backward.

    // Super simple: the loop instruction jumps backward.
    // We just test that it does jump backward and eventually we hit return.
    //
    //  0: constant 1      (3 bytes)
    //  3: constant 2      (3 bytes)
    //  6: add              (1 byte)  → stack: [3]
    //  7: return           (1 byte)
    //
    // We'll create a chunk that uses loop to jump to a return:
    //  0: constant 42     (3 bytes)
    //  3: jump +3         (3 bytes) → offset 9
    //  6: return           (1 byte)  ← loop target
    //  7: nop (shouldn't exist)
    //  ...but we need something at offset 7-8 for the loop
    //
    // OK simplest possible: verify loop jumps back properly.
    //  0: constant 42     (3 bytes)
    //  3: jump +4         (3 bytes) → offset 10
    //  6: pop              (1 byte)  ← loop will jump here? No.
    //
    // Let me just do a practical loop test:
    // Count down from 3, summing: result = 3 + 2 + 1 = 6
    //
    // Stack layout: [acc] [counter]
    //  0: constant 0         (3 bytes)  ; acc (slot 0)
    //  3: constant 3         (3 bytes)  ; counter (slot 1)
    //
    // loop_top = offset 6:
    //  6: get_local 1        (3 bytes)  ; push counter
    //  9: constant 0         (3 bytes)  ; push 0
    // 12: equal              (1 byte)   ; counter == 0?
    // 13: jump_if_false +4   (3 bytes)  ; if not zero, skip to offset 20
    // 16: get_local 0        (3 bytes)  ; push accumulator
    // 19: return             (1 byte)   ; done!
    //
    // 20: get_local 0        (3 bytes)  ; push acc
    // 23: get_local 1        (3 bytes)  ; push counter
    // 26: add                (1 byte)   ; acc + counter
    // 27: set_local 0        (3 bytes)  ; acc = acc + counter
    //
    // 30: get_local 1        (3 bytes)  ; push counter
    // 33: constant 1         (3 bytes)  ; push 1
    // 36: subtract           (1 byte)   ; counter - 1
    // 37: set_local 1        (3 bytes)  ; counter = counter - 1
    //
    // 40: loop 37            (3 bytes)  ; jump back to offset 6 (43 - 37 = 6)
    //
    // Total = offset 43, loop operand = 37.

    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    // Constants
    const c0 = try chunk.findOrAddConstant(.{ .int = 0 }); // index 0
    const c3 = try chunk.findOrAddConstant(.{ .int = 3 }); // index 1
    const c1 = try chunk.findOrAddConstant(.{ .int = 1 }); // index 2

    // offset 0: acc = 0 (slot 0)
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c0, 1);

    // offset 3: counter = 3 (slot 1)
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c3, 1);

    // offset 6 (loop_top): get_local 1 (counter)
    try chunk.writeOp(.get_local, 1);
    try chunk.writeU16(1, 1);

    // offset 9: push 0
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c0, 1);

    // offset 12: equal
    try chunk.writeOp(.equal, 1);

    // offset 13: jump_if_false → absolute offset 20
    try chunk.writeOp(.jump_if_false, 1);
    try chunk.writeU16(20, 1);

    // offset 16: get_local 0 (acc) — this is the "done" path
    try chunk.writeOp(.get_local, 1);
    try chunk.writeU16(0, 1);

    // offset 19: return
    try chunk.writeOp(.@"return", 1);

    // offset 20: get_local 0 (acc)
    try chunk.writeOp(.get_local, 1);
    try chunk.writeU16(0, 1);

    // offset 23: get_local 1 (counter)
    try chunk.writeOp(.get_local, 1);
    try chunk.writeU16(1, 1);

    // offset 26: add
    try chunk.writeOp(.add, 1);

    // offset 27: set_local 0 (acc = acc + counter)
    try chunk.writeOp(.set_local, 1);
    try chunk.writeU16(0, 1);

    // offset 30: get_local 1 (counter)
    try chunk.writeOp(.get_local, 1);
    try chunk.writeU16(1, 1);

    // offset 33: push 1
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c1, 1);

    // offset 36: subtract
    try chunk.writeOp(.subtract, 1);

    // offset 37: set_local 1 (counter = counter - 1)
    try chunk.writeOp(.set_local, 1);
    try chunk.writeU16(1, 1);

    // offset 40: loop → jumps to absolute offset 6
    try chunk.writeOp(.loop, 1);
    try chunk.writeU16(6, 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .int = 6 })); // 3 + 2 + 1
}

// ===========================================================================
// Step 5: Struct opcodes
// ===========================================================================

test "construct then get_field returns constructor field" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const c_id = try chunk.findOrAddConstant(.{ .int = 7 });
    const c_name_val = try chunk.findOrAddConstant(.{ .string = "Ada" });
    const c_field_name = try chunk.findOrAddConstant(.{ .string = "name" });

    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_id, 1);
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_name_val, 1);
    try chunk.writeOp(.construct, 1);
    try chunk.writeU16(0, 1);
    try chunk.writeOp(.get_field, 1);
    try chunk.writeU16(c_field_name, 1);
    try chunk.writeOp(.@"return", 1);

    const field_names = [_][]const u8{ "id", "name" };
    const body_field_names = [_][]const u8{};
    const defs = [_]StructDef{.{
        .name = "User",
        .field_names = field_names[0..],
        .body_field_names = body_field_names[0..],
        .kind = StructKind.case,
        .body_default_fn = null,
    }};

    const result = try runChunkWithStructDefs(&chunk, defs[0..]);
    try std.testing.expect(result.eql(.{ .string = "Ada" }));
}

test "set_field mutates instance field" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const c_id = try chunk.findOrAddConstant(.{ .int = 1 });
    const c_name_initial = try chunk.findOrAddConstant(.{ .string = "Alice" });
    const c_name_updated = try chunk.findOrAddConstant(.{ .string = "Bob" });
    const c_global_name = try chunk.findOrAddConstant(.{ .string = "u" });
    const c_field_name = try chunk.findOrAddConstant(.{ .string = "name" });

    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_id, 1);
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_name_initial, 1);
    try chunk.writeOp(.construct, 1);
    try chunk.writeU16(0, 1);

    try chunk.writeOp(.define_global, 1);
    try chunk.writeU16(c_global_name, 1);

    try chunk.writeOp(.get_global, 1);
    try chunk.writeU16(c_global_name, 1);
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_name_updated, 1);
    try chunk.writeOp(.set_field, 1);
    try chunk.writeU16(c_field_name, 1);
    try chunk.writeOp(.pop, 1);

    try chunk.writeOp(.get_global, 1);
    try chunk.writeU16(c_global_name, 1);
    try chunk.writeOp(.get_field, 1);
    try chunk.writeU16(c_field_name, 1);
    try chunk.writeOp(.@"return", 1);

    const field_names = [_][]const u8{ "id", "name" };
    const body_field_names = [_][]const u8{};
    const defs = [_]StructDef{.{
        .name = "User",
        .field_names = field_names[0..],
        .body_field_names = body_field_names[0..],
        .kind = StructKind.plain,
        .body_default_fn = null,
    }};

    const result = try runChunkWithStructDefs(&chunk, defs[0..]);
    try std.testing.expect(result.eql(.{ .string = "Bob" }));
}

test "get_field on missing name is UndefinedField" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const c_id = try chunk.findOrAddConstant(.{ .int = 1 });
    const c_name_val = try chunk.findOrAddConstant(.{ .string = "Alice" });
    const c_missing_field = try chunk.findOrAddConstant(.{ .string = "email" });

    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_id, 1);
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_name_val, 1);
    try chunk.writeOp(.construct, 1);
    try chunk.writeU16(0, 1);
    try chunk.writeOp(.get_field, 1);
    try chunk.writeU16(c_missing_field, 1);
    try chunk.writeOp(.@"return", 1);

    const field_names = [_][]const u8{ "id", "name" };
    const body_field_names = [_][]const u8{};
    const defs = [_]StructDef{.{
        .name = "User",
        .field_names = field_names[0..],
        .body_field_names = body_field_names[0..],
        .kind = StructKind.plain,
        .body_default_fn = null,
    }};

    try std.testing.expectError(error.UndefinedField, runChunkWithStructDefs(&chunk, defs[0..]));
}

test "get_field on non-struct is TypeError" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const c_num = try chunk.findOrAddConstant(.{ .int = 123 });
    const c_field_name = try chunk.findOrAddConstant(.{ .string = "name" });

    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c_num, 1);
    try chunk.writeOp(.get_field, 1);
    try chunk.writeU16(c_field_name, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.TypeError, runChunk(&chunk));
}

// ===========================================================================
// Step 6: Compiler struct tests
// ===========================================================================

/// Helper token with .identifier type.
fn ident(lexeme: []const u8) Token {
    return .{ .type = .identifier, .lexeme = lexeme, .line = 1 };
}

/// Dummy type annotation (compiler ignores it for struct fields).
fn dummyType() ast.PipeTypeAnnotation {
    return .{ .named = ident("Int") };
}

/// Run a series of statements through the compiler, then compile+return a
/// final expression, and execute the result with the VM.
fn runCompiled(
    allocator: std.mem.Allocator,
    statements: []const ast.Statement,
    final_expr: ast.Expression,
) !Value {
    var program = Program.init(allocator);
    defer program.deinit();

    var compiler = Compiler.init(&program, allocator);
    defer compiler.deinit();

    for (statements) |stmt| {
        try compiler.compileStatement(stmt);
    }
    try compiler.compileExpression(final_expr);
    try compiler.emitReturn(1);

    var aw: std.Io.Writer.Allocating = .init(allocator);
    defer aw.deinit();
    const ctx = RuntimeContext{ .writer = &aw.writer };
    var vm = Vm.init(&program, ctx, allocator);
    defer vm.deinit();
    return try vm.run();
}

test "compiler: struct declaration and construction roundtrip" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct User(id: Int, name: Str);
    const fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("id"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
        .{ .name = ident("name"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };
    const struct_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("User"),
        .fields = fields[0..],
        .body_fields = &.{},
        .kind = .case,
        .methods = &.{},
    } };

    // User(7, "Ada")
    const args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("7"), .value = .{ .int = 7 } } },
        .{ .literal = .{ .token = ident("Ada"), .value = .{ .string = "Ada" } } },
    };
    const si = try allocator.create(ast.Expression.StructInit);
    si.* = .{ .name = ident("User"), .args = args[0..] };

    const result = try runCompiled(allocator, &.{struct_decl}, .{ .struct_init = si });
    try std.testing.expect(result == .struct_instance);
    try std.testing.expectEqualStrings("User", result.struct_instance.type_name);
}

test "compiler: field access on constructed instance" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct User(id: Int, name: Str);
    const fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("id"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
        .{ .name = ident("name"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };
    const struct_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("User"),
        .fields = fields[0..],
        .body_fields = &.{},
        .kind = .case,
        .methods = &.{},
    } };

    // var u = User(7, "Ada");
    const args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("7"), .value = .{ .int = 7 } } },
        .{ .literal = .{ .token = ident("Ada"), .value = .{ .string = "Ada" } } },
    };
    const si = try allocator.create(ast.Expression.StructInit);
    si.* = .{ .name = ident("User"), .args = args[0..] };
    const var_decl = ast.Statement{ .var_declaration = .{
        .name = ident("u"),
        .type_annotation = null,
        .initializer = .{ .struct_init = si },
        .mutability = .constant,
    } };

    // u.name
    const fa = try allocator.create(ast.Expression.FieldAccess);
    fa.* = .{ .object = .{ .variable = .{ .token = ident("u") } }, .name = ident("name") };

    const result = try runCompiled(allocator, &.{ struct_decl, var_decl }, .{ .field_access = fa });
    try std.testing.expect(result.eql(.{ .string = "Ada" }));
}

test "compiler: field assignment and re-read" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct User(id: Int, name: Str);
    const fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("id"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
        .{ .name = ident("name"), .type_annotation = dummyType(), .mutability = .mutable, .default_value = null },
    };
    const struct_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("User"),
        .fields = fields[0..],
        .body_fields = &.{},
        .kind = .plain,
        .methods = &.{},
    } };

    // var u = User(1, "Alice");
    const init_args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("1"), .value = .{ .int = 1 } } },
        .{ .literal = .{ .token = ident("Alice"), .value = .{ .string = "Alice" } } },
    };
    const si = try allocator.create(ast.Expression.StructInit);
    si.* = .{ .name = ident("User"), .args = init_args[0..] };
    const var_decl = ast.Statement{ .var_declaration = .{
        .name = ident("u"),
        .type_annotation = null,
        .initializer = .{ .struct_init = si },
        .mutability = .mutable,
    } };

    // u.name = "Bob"  (expression statement)
    const assign = try allocator.create(ast.Expression.FieldAssignment);
    assign.* = .{
        .object = .{ .variable = .{ .token = ident("u") } },
        .name = ident("name"),
        .value = .{ .literal = .{ .token = ident("Bob"), .value = .{ .string = "Bob" } } },
    };
    const assign_stmt = ast.Statement{ .expression = .{ .field_assignment = assign } };

    // u.name
    const fa = try allocator.create(ast.Expression.FieldAccess);
    fa.* = .{ .object = .{ .variable = .{ .token = ident("u") } }, .name = ident("name") };

    const result = try runCompiled(allocator, &.{ struct_decl, var_decl, assign_stmt }, .{ .field_access = fa });
    try std.testing.expect(result.eql(.{ .string = "Bob" }));
}

test "compiler: nested field access a.b.c" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct Point(x: Int);
    const point_fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("x"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };
    const point_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("Point"),
        .fields = point_fields[0..],
        .body_fields = &.{},
        .kind = .case,
        .methods = &.{},
    } };

    // struct Line(start: Point);
    const line_fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("start"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };
    const line_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("Line"),
        .fields = line_fields[0..],
        .body_fields = &.{},
        .kind = .case,
        .methods = &.{},
    } };

    // var p = Point(3);
    const p_args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("3"), .value = .{ .int = 3 } } },
    };
    const p_si = try allocator.create(ast.Expression.StructInit);
    p_si.* = .{ .name = ident("Point"), .args = p_args[0..] };
    const p_decl = ast.Statement{ .var_declaration = .{
        .name = ident("p"),
        .type_annotation = null,
        .initializer = .{ .struct_init = p_si },
        .mutability = .constant,
    } };

    // var l = Line(p);
    const l_args = [_]ast.Expression{
        .{ .variable = .{ .token = ident("p") } },
    };
    const l_si = try allocator.create(ast.Expression.StructInit);
    l_si.* = .{ .name = ident("Line"), .args = l_args[0..] };
    const l_decl = ast.Statement{ .var_declaration = .{
        .name = ident("l"),
        .type_annotation = null,
        .initializer = .{ .struct_init = l_si },
        .mutability = .constant,
    } };

    // l.start.x
    const fa_start = try allocator.create(ast.Expression.FieldAccess);
    fa_start.* = .{ .object = .{ .variable = .{ .token = ident("l") } }, .name = ident("start") };
    const fa_x = try allocator.create(ast.Expression.FieldAccess);
    fa_x.* = .{ .object = .{ .field_access = fa_start }, .name = ident("x") };

    const result = try runCompiled(allocator, &.{ point_decl, line_decl, p_decl, l_decl }, .{ .field_access = fa_x });
    try std.testing.expect(result.eql(.{ .int = 3 }));
}

// ===========================================================================
// Step 7: Enum variant constructors
// ===========================================================================

test "compiler: enum zero-field variant construction" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // enum Color { Red, Green }
    const variants = [_]ast.Statement.Variant{
        .{ .name = ident("Red"), .fields = &.{} },
        .{ .name = ident("Green"), .fields = &.{} },
    };
    const enum_decl = ast.Statement{ .enum_declaration = .{
        .name = ident("Color"),
        .is_error = false,
        .variants = &variants,
    } };

    // Color.Red()
    const fa = try allocator.create(ast.Expression.FieldAccess);
    fa.* = .{ .object = .{ .variable = .{ .token = ident("Color") } }, .name = ident("Red") };
    const call = try allocator.create(ast.Expression.FnCall);
    call.* = .{ .callee = .{ .field_access = fa }, .args = &.{} };

    const result = try runCompiled(allocator, &.{enum_decl}, .{ .fn_call = call });
    try std.testing.expect(result == .struct_instance);
    try std.testing.expectEqualStrings("Color.Red", result.struct_instance.type_name);
    try std.testing.expectEqual(@as(usize, 0), result.struct_instance.field_values.len);
}

test "compiler: enum variant with fields" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // enum Shape { Circle(const radius: Int) }
    const circle_fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("radius"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };
    const variants = [_]ast.Statement.Variant{
        .{ .name = ident("Circle"), .fields = &circle_fields },
    };
    const enum_decl = ast.Statement{ .enum_declaration = .{
        .name = ident("Shape"),
        .is_error = false,
        .variants = &variants,
    } };

    // Shape.Circle(5)
    const fa = try allocator.create(ast.Expression.FieldAccess);
    fa.* = .{ .object = .{ .variable = .{ .token = ident("Shape") } }, .name = ident("Circle") };
    const args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("5"), .value = .{ .int = 5 } } },
    };
    const call = try allocator.create(ast.Expression.FnCall);
    call.* = .{ .callee = .{ .field_access = fa }, .args = &args };

    const result = try runCompiled(allocator, &.{enum_decl}, .{ .fn_call = call });
    try std.testing.expect(result == .struct_instance);
    try std.testing.expectEqualStrings("Shape.Circle", result.struct_instance.type_name);
    try std.testing.expectEqual(@as(usize, 1), result.struct_instance.field_values.len);
    try std.testing.expect(result.struct_instance.field_values[0].eql(.{ .int = 5 }));
}

test "compiler: qualified enum access resolves to correct variant" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // enum Color { Red, Green }
    const variants = [_]ast.Statement.Variant{
        .{ .name = ident("Red"), .fields = &.{} },
        .{ .name = ident("Green"), .fields = &.{} },
    };
    const enum_decl = ast.Statement{ .enum_declaration = .{
        .name = ident("Color"),
        .is_error = false,
        .variants = &variants,
    } };

    // Color.Green()  — must resolve to Green, not Red
    const fa = try allocator.create(ast.Expression.FieldAccess);
    fa.* = .{ .object = .{ .variable = .{ .token = ident("Color") } }, .name = ident("Green") };
    const call = try allocator.create(ast.Expression.FnCall);
    call.* = .{ .callee = .{ .field_access = fa }, .args = &.{} };

    const result = try runCompiled(allocator, &.{enum_decl}, .{ .fn_call = call });
    try std.testing.expect(result == .struct_instance);
    try std.testing.expectEqualStrings("Color.Green", result.struct_instance.type_name);
}

test "compiler: nested enum coercion" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // enum Inner { A }
    const inner_variants = [_]ast.Statement.Variant{
        .{ .name = ident("A"), .fields = &.{} },
    };
    const inner_decl = ast.Statement{ .enum_declaration = .{
        .name = ident("Inner"),
        .is_error = false,
        .variants = &inner_variants,
    } };

    // enum Outer { Inner, B }
    // "Inner" has zero declared fields but matches a known enum → single-field wrapper
    const outer_variants = [_]ast.Statement.Variant{
        .{ .name = ident("Inner"), .fields = &.{} },
        .{ .name = ident("B"), .fields = &.{} },
    };
    const outer_decl = ast.Statement{ .enum_declaration = .{
        .name = ident("Outer"),
        .is_error = false,
        .variants = &outer_variants,
    } };

    // Inner.A()
    const inner_fa = try allocator.create(ast.Expression.FieldAccess);
    inner_fa.* = .{ .object = .{ .variable = .{ .token = ident("Inner") } }, .name = ident("A") };
    const inner_call = try allocator.create(ast.Expression.FnCall);
    inner_call.* = .{ .callee = .{ .field_access = inner_fa }, .args = &.{} };

    // Outer.Inner(Inner.A())
    const outer_fa = try allocator.create(ast.Expression.FieldAccess);
    outer_fa.* = .{ .object = .{ .variable = .{ .token = ident("Outer") } }, .name = ident("Inner") };
    const outer_args = [_]ast.Expression{.{ .fn_call = inner_call }};
    const outer_call = try allocator.create(ast.Expression.FnCall);
    outer_call.* = .{ .callee = .{ .field_access = outer_fa }, .args = &outer_args };

    const result = try runCompiled(allocator, &.{ inner_decl, outer_decl }, .{ .fn_call = outer_call });
    try std.testing.expect(result == .struct_instance);
    try std.testing.expectEqualStrings("Outer.Inner", result.struct_instance.type_name);
    try std.testing.expectEqual(@as(usize, 1), result.struct_instance.field_values.len);
    try std.testing.expect(result.struct_instance.field_values[0] == .struct_instance);
    try std.testing.expectEqualStrings("Inner.A", result.struct_instance.field_values[0].struct_instance.type_name);
}

// ===========================================================================
// Step 7: Method dispatch
// ===========================================================================

test "compiler: no-arg method reads self field" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct Counter(const val: Int) {
    //     fn get() { return self.val; }
    // }
    const fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("val"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };

    // Body: return self.val;
    const self_fa = try allocator.create(ast.Expression.FieldAccess);
    self_fa.* = .{ .object = .{ .variable = .{ .token = ident("self") } }, .name = ident("val") };
    const ret_stmt = ast.Statement{ .@"return" = .{
        .token = ident("return"),
        .value = .{ .field_access = self_fa },
    } };

    const get_method = ast.Statement.FnDeclaration{
        .name = ident("get"),
        .params = &.{},
        .return_type = null,
        .body = &.{ret_stmt},
    };

    const struct_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("Counter"),
        .fields = &fields,
        .body_fields = &.{},
        .kind = .plain,
        .methods = &.{get_method},
    } };

    // const c = Counter(42);
    const init_args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("42"), .value = .{ .int = 42 } } },
    };
    const si = try allocator.create(ast.Expression.StructInit);
    si.* = .{ .name = ident("Counter"), .args = &init_args };
    const var_decl = ast.Statement{ .var_declaration = .{
        .name = ident("c"),
        .type_annotation = null,
        .initializer = .{ .struct_init = si },
        .mutability = .constant,
    } };

    // c.get()
    const method_fa = try allocator.create(ast.Expression.FieldAccess);
    method_fa.* = .{ .object = .{ .variable = .{ .token = ident("c") } }, .name = ident("get") };
    const call = try allocator.create(ast.Expression.FnCall);
    call.* = .{ .callee = .{ .field_access = method_fa }, .args = &.{} };

    const result = try runCompiled(allocator, &.{ struct_decl, var_decl }, .{ .fn_call = call });
    try std.testing.expect(result.eql(.{ .int = 42 }));
}

test "compiler: method with extra argument" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct Counter(const val: Int) {
    //     fn add(n: Int) { return self.val + n; }
    // }
    const fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("val"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };

    // Body: return self.val + n;
    const self_fa = try allocator.create(ast.Expression.FieldAccess);
    self_fa.* = .{ .object = .{ .variable = .{ .token = ident("self") } }, .name = ident("val") };
    const add_bin = try allocator.create(ast.Expression.Binary);
    add_bin.* = .{
        .left = .{ .field_access = self_fa },
        .operator = .{ .type = .plus, .lexeme = "+", .line = 1 },
        .right = .{ .variable = .{ .token = ident("n") } },
    };
    const ret_stmt = ast.Statement{ .@"return" = .{
        .token = ident("return"),
        .value = .{ .binary = add_bin },
    } };

    var add_params = [_]ast.Param{
        .{ .name = ident("n"), .type_annotation = dummyType() },
    };
    const add_method = ast.Statement.FnDeclaration{
        .name = ident("add"),
        .params = &add_params,
        .return_type = null,
        .body = &.{ret_stmt},
    };

    const struct_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("Counter"),
        .fields = &fields,
        .body_fields = &.{},
        .kind = .plain,
        .methods = &.{add_method},
    } };

    // const c = Counter(10);
    const init_args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("10"), .value = .{ .int = 10 } } },
    };
    const si = try allocator.create(ast.Expression.StructInit);
    si.* = .{ .name = ident("Counter"), .args = &init_args };
    const var_decl = ast.Statement{ .var_declaration = .{
        .name = ident("c"),
        .type_annotation = null,
        .initializer = .{ .struct_init = si },
        .mutability = .constant,
    } };

    // c.add(5)
    const method_fa = try allocator.create(ast.Expression.FieldAccess);
    method_fa.* = .{ .object = .{ .variable = .{ .token = ident("c") } }, .name = ident("add") };
    const call_args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("5"), .value = .{ .int = 5 } } },
    };
    const call = try allocator.create(ast.Expression.FnCall);
    call.* = .{ .callee = .{ .field_access = method_fa }, .args = &call_args };

    const result = try runCompiled(allocator, &.{ struct_decl, var_decl }, .{ .fn_call = call });
    try std.testing.expect(result.eql(.{ .int = 15 }));
}

test "compiler: method dispatches to correct receiver" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // struct Counter(const val: Int) {
    //     fn get() { return self.val; }
    // }
    // Two instances — verify self is bound to the right one.
    const fields = [_]ast.Statement.FieldDeclaration{
        .{ .name = ident("val"), .type_annotation = dummyType(), .mutability = .constant, .default_value = null },
    };

    const self_fa = try allocator.create(ast.Expression.FieldAccess);
    self_fa.* = .{ .object = .{ .variable = .{ .token = ident("self") } }, .name = ident("val") };
    const ret_stmt = ast.Statement{ .@"return" = .{
        .token = ident("return"),
        .value = .{ .field_access = self_fa },
    } };
    const get_method = ast.Statement.FnDeclaration{
        .name = ident("get"),
        .params = &.{},
        .return_type = null,
        .body = &.{ret_stmt},
    };

    const struct_decl = ast.Statement{ .struct_declaration = .{
        .name = ident("Counter"),
        .fields = &fields,
        .body_fields = &.{},
        .kind = .plain,
        .methods = &.{get_method},
    } };

    // const a = Counter(1);
    const a_args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("1"), .value = .{ .int = 1 } } },
    };
    const a_si = try allocator.create(ast.Expression.StructInit);
    a_si.* = .{ .name = ident("Counter"), .args = &a_args };
    const a_decl = ast.Statement{ .var_declaration = .{
        .name = ident("a"),
        .type_annotation = null,
        .initializer = .{ .struct_init = a_si },
        .mutability = .constant,
    } };

    // const b = Counter(99);
    const b_args = [_]ast.Expression{
        .{ .literal = .{ .token = ident("99"), .value = .{ .int = 99 } } },
    };
    const b_si = try allocator.create(ast.Expression.StructInit);
    b_si.* = .{ .name = ident("Counter"), .args = &b_args };
    const b_decl = ast.Statement{ .var_declaration = .{
        .name = ident("b"),
        .type_annotation = null,
        .initializer = .{ .struct_init = b_si },
        .mutability = .constant,
    } };

    // b.get() → 99 (not 1)
    const method_fa = try allocator.create(ast.Expression.FieldAccess);
    method_fa.* = .{ .object = .{ .variable = .{ .token = ident("b") } }, .name = ident("get") };
    const call = try allocator.create(ast.Expression.FnCall);
    call.* = .{ .callee = .{ .field_access = method_fa }, .args = &.{} };

    const result = try runCompiled(allocator, &.{ struct_decl, a_decl, b_decl }, .{ .fn_call = call });
    try std.testing.expect(result.eql(.{ .int = 99 }));
}
