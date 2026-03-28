const std = @import("std");
const vm_pkg = @import("pipe").vm;
const Chunk = vm_pkg.Chunk;
const OpCode = vm_pkg.OpCode;
const Vm = vm_pkg.Vm;
const Program = vm_pkg.Program;
const Value = @import("pipe").ast.Value;

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
    var vm = Vm.init(&program, std.testing.allocator);
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
    var vm = Vm.init(&program, std.testing.allocator);
    defer vm.deinit();

    var buf = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer buf.deinit();
    vm.setOutputWriter(&buf.writer);

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

    const idx = try chunk.addConstant(.{ .int = 42 });
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

    const idx = try chunk.addConstant(.{ .int = 7 });
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

    const idx = try chunk.addConstant(.{ .int = 0 });
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

    const idx0 = try chunk.addConstant(.{ .int = 99 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(idx0, 1);

    const idx1 = try chunk.addConstant(.{ .int = 42 });
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

    const a = try chunk.addConstant(.{ .int = 10 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 32 });
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

    const a = try chunk.addConstant(.{ .int = 50 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 8 });
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

    const a = try chunk.addConstant(.{ .int = 6 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 7 });
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

    const a = try chunk.addConstant(.{ .int = 84 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 2 });
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

    const a = try chunk.addConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 0 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.divide, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.DivisionByZero, runChunk(&chunk));
}

test "arithmetic type error" {
    // true + 1 → TypeError
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.true, 1);

    const b = try chunk.addConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.add, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.TypeError, runChunk(&chunk));
}

test "equal integers" {
    // 42 == 42 → true
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const a = try chunk.addConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 42 });
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

    const a = try chunk.addConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 2 });
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

    const a = try chunk.addConstant(.{ .int = 5 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 3 });
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

    const a = try chunk.addConstant(.{ .int = 3 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(a, 1);

    const b = try chunk.addConstant(.{ .int = 5 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.less, 1);
    try chunk.writeOp(.@"return", 1);

    const result = try runChunk(&chunk);
    try std.testing.expect(result.eql(.{ .boolean = true }));
}

test "comparison type error" {
    // true > 1 → TypeError
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.true, 1);

    const b = try chunk.addConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(b, 1);

    try chunk.writeOp(.greater, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.TypeError, runChunk(&chunk));
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

    const c2 = try chunk.addConstant(.{ .int = 2 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c2, 1);

    const c3 = try chunk.addConstant(.{ .int = 3 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(c3, 1);

    try chunk.writeOp(.add, 1);

    const c4 = try chunk.addConstant(.{ .int = 4 });
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

    const name_idx = try chunk.addConstant(.{ .string = "x" });
    const val_idx = try chunk.addConstant(.{ .int = 42 });

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

    const name_idx = try chunk.addConstant(.{ .string = "x" });
    const val10 = try chunk.addConstant(.{ .int = 10 });
    const val20 = try chunk.addConstant(.{ .int = 20 });

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

    const name_idx = try chunk.addConstant(.{ .string = "nope" });
    try chunk.writeOp(.get_global, 1);
    try chunk.writeU16(name_idx, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.UndefinedVariable, runChunk(&chunk));
}

test "set undefined global is error" {
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    const name_idx = try chunk.addConstant(.{ .string = "nope" });
    const val = try chunk.addConstant(.{ .int = 1 });

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
    const val = try chunk.addConstant(.{ .int = 42 });
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
    const val10 = try chunk.addConstant(.{ .int = 10 });
    try chunk.writeOp(.constant, 1);
    try chunk.writeU16(val10, 1); // slot 1

    const val99 = try chunk.addConstant(.{ .int = 99 });
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

    const c1 = try chunk.addConstant(.{ .int = 1 });
    try chunk.writeOp(.constant, 1); // offset 0
    try chunk.writeU16(c1, 1); // offset 1-2

    try chunk.writeOp(.jump, 1); // offset 3
    try chunk.writeU16(10, 1); // offset 4-5, jump to absolute offset 10

    const c_bad = try chunk.addConstant(.{ .int = 999 });
    try chunk.writeOp(.constant, 1); // offset 6 (skipped)
    try chunk.writeU16(c_bad, 1); // offset 7-8 (skipped)
    try chunk.writeOp(.@"return", 1); // offset 9 (skipped)

    const c3 = try chunk.addConstant(.{ .int = 3 });
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

    const c_bad = try chunk.addConstant(.{ .int = 999 });
    try chunk.writeOp(.constant, 1); // offset 4 (skipped)
    try chunk.writeU16(c_bad, 1); // offset 5-6 (skipped)
    try chunk.writeOp(.@"return", 1); // offset 7 (skipped)

    const c42 = try chunk.addConstant(.{ .int = 42 });
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

    const c42 = try chunk.addConstant(.{ .int = 42 });
    try chunk.writeOp(.constant, 1); // offset 4
    try chunk.writeU16(c42, 1);
    try chunk.writeOp(.@"return", 1); // offset 7

    const c_bad = try chunk.addConstant(.{ .int = 999 });
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
    const c0 = try chunk.addConstant(.{ .int = 0 }); // index 0
    const c3 = try chunk.addConstant(.{ .int = 3 }); // index 1
    const c1 = try chunk.addConstant(.{ .int = 1 }); // index 2

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

test "stack underflow is error" {
    // Try to add with empty stack
    var chunk = Chunk.init(std.testing.allocator);
    defer chunk.deinit();

    try chunk.writeOp(.add, 1);
    try chunk.writeOp(.@"return", 1);

    try std.testing.expectError(error.StackUnderflow, runChunk(&chunk));
}
