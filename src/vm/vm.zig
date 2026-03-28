const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("../ast.zig").Value;
const FnObject = @import("program.zig").FnObject;
const Program = @import("program.zig").Program;

pub const VmError = error{
    DivisionByZero,
    StackUnderflow, // Pop from empty stack
    TypeError, // wront types for arithmetic/negate/table
    UndefinedVariable,
    ArityMismatch,
    NotCallable, // Not a function/method that we can call
};

const MAX_FRAMES = 64;

const CallFrame = struct {
    chunk: *const Chunk,
    ip: usize,
    base_slot: usize, // Where this frame's locals start on the value stack
};

pub const Vm = struct {
    // Program containing the Chunk + VM tables
    program: *const Program,

    // Value stack
    stack: std.ArrayList(Value),

    // Call frame stack
    frames: [MAX_FRAMES]CallFrame,
    frame_count: usize = 0,

    // Global variables table
    // name -> value
    globals: std.StringHashMapUnmanaged(Value),

    allocator: std.mem.Allocator,

    pub fn init(program: *const Program, allocator: std.mem.Allocator) Vm {
        return .{
            .program = program,
            .stack = .{},
            .frames = undefined,
            .frame_count = 0,
            .globals = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Vm) void {
        self.stack.deinit(self.allocator);
        self.globals.deinit(self.allocator);
    }

    pub fn run(self: *Vm) !Value {
        // Setup initial frame for top-level
        self.frames[0] = .{ .chunk = &self.program.chunk, .ip = 0, .base_slot = 0 };
        self.frame_count = 1;
        return self.execute();
    }

    fn execute(self: *Vm) !Value {
        while (self.frame_count > 0) {
            const frame = self.currentFrame();
            const op: OpCode = @enumFromInt(frame.chunk.code.items[frame.ip]);
            frame.ip += 1;

            switch (op) {
                .constant => {
                    // Read pool index from chunk
                    const idx = readU16(frame.chunk, &frame.ip);
                    try self.push(frame.chunk.constants.items[idx]);
                },
                .true => try self.push(.{ .boolean = true }),
                .false => try self.push(.{ .boolean = false }),
                .null => try self.push(.null),
                .unit => try self.push(.unit),
                .pop => _ = try self.pop(),
                .negate => {
                    const val = try self.pop();
                    if (!val.isNumber()) {
                        return VmError.TypeError;
                    }

                    try self.push(switch (val) {
                        .int => |n| .{ .int = -n },
                        else => unreachable,
                    });
                },
                .not => {
                    const val = try self.pop();
                    try self.push(.{ .boolean = !val.isTruthy() });
                },
                .add, .subtract, .multiply, .divide => {
                    const b = try self.pop(); // right
                    const a = try self.pop(); // left

                    if (!a.isNumber() or !b.isNumber()) {
                        return error.TypeError;
                    }

                    try self.push(.{ .int = switch (op) {
                        .add => a.int + b.int,
                        .subtract => a.int - b.int,
                        .multiply => a.int * b.int,
                        .divide => if (b.int == 0) return error.DivisionByZero else @divTrunc(a.int, b.int),
                        else => unreachable,
                    } });
                },
                .equal => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(.{ .boolean = a.eql(b) });
                },
                .greater, .less => {
                    // Read operands from stack
                    const b = try self.pop();
                    const a = try self.pop();

                    if (!a.isNumber() or !b.isNumber()) {
                        return error.TypeError;
                    }

                    // Compare them and push the result
                    try self.push(.{ .boolean = switch (op) {
                        .greater => a.int > b.int,
                        .less => a.int < b.int,
                        else => unreachable,
                    } });
                },
                .define_global => {
                    // Read pool index to retrieve the name
                    const idx = readU16(frame.chunk, &frame.ip);
                    const name = frame.chunk.constants.items[idx].string;

                    const val = try self.pop();
                    try self.globals.put(self.allocator, name, val);
                },
                .get_global => {
                    const idx = readU16(frame.chunk, &frame.ip);
                    const val = self.globals.get(frame.chunk.constants.items[idx].string) orelse return error.UndefinedVariable;
                    try self.push(val);
                },
                .set_global => {
                    const idx = readU16(frame.chunk, &frame.ip);
                    const name = frame.chunk.constants.items[idx].string;

                    if (self.globals.getPtr(name)) |ptr| {
                        ptr.* = self.stack.getLast();
                    } else {
                        return error.UndefinedVariable;
                    }
                },
                .get_local => {
                    const idx = frame.base_slot + readU16(frame.chunk, &frame.ip);
                    try self.push(self.stack.items[idx]);
                },
                .set_local => {
                    const idx = frame.base_slot + readU16(frame.chunk, &frame.ip);
                    self.stack.items[idx] = self.stack.getLast();
                },
                .jump => frame.ip = readU16(frame.chunk, &frame.ip),
                .jump_if_false => {
                    // Read offset
                    const offset = readU16(frame.chunk, &frame.ip);

                    // Evalute condition on stack
                    const condition = try self.pop();
                    if (!condition.isTruthy()) {
                        frame.ip = offset;
                    }
                },
                .loop => frame.ip = readU16(frame.chunk, &frame.ip),
                .@"return" => {
                    // Pop the return value (or unit if stack is at base)
                    const ret_value = if (self.stack.items.len > frame.base_slot)
                        try self.pop()
                    else
                        Value.unit;

                    // Remove all locals of the current stack frame
                    self.stack.shrinkRetainingCapacity(frame.base_slot);
                    self.frame_count -= 1;

                    // Top-level frame, return the value
                    if (self.frame_count == 0) {
                        return ret_value;
                    }

                    // Push the return value on the stack of the caller
                    try self.push(ret_value);
                },
                .call => {
                    // Read function arity
                    const arity = readU8(frame.chunk, &frame.ip);

                    // Peak the callee to check if we can cast it to *FnObject
                    const base_slot = self.stack.items.len - 1 - arity;
                    const callee = self.stack.items[base_slot];

                    if (callee != .vm_function) {
                        return error.NotCallable;
                    }
                    const fn_obj: *const FnObject = &self.program.functions.items[callee.vm_function];

                    if (fn_obj.arity != arity) {
                        return error.ArityMismatch;
                    }

                    // Push the frame
                    self.frames[self.frame_count] = .{
                        .chunk = &fn_obj.chunk,
                        .ip = 0,
                        .base_slot = base_slot,
                    };
                    self.frame_count += 1;
                },
            }
        }

        return .unit;
    }

    // NOTE: -- Helpers

    // Pushes the value onto the stack
    fn push(self: *Vm, value: Value) !void {
        try self.stack.append(self.allocator, value);
    }

    // Pops the value from the stack
    fn pop(self: *Vm) !Value {
        if (self.stack.items.len == 0) {
            return error.StackUnderflow;
        }

        // Unwrap the nullable or crash. Life is hard.
        return self.stack.pop().?;
    }

    // Read a 8-bit value from the code
    // and advances the instruction pointer
    fn readU8(chunk: *const Chunk, ip: *usize) u8 {
        const value = chunk.code.items[ip.*];
        ip.* += 1;
        return value;
    }

    // Reas a 16-bit value from the code
    // and advances the instruction pointer
    fn readU16(chunk: *const Chunk, ip: *usize) u16 {
        // Decode high and low bytes
        const hi = chunk.code.items[ip.*];
        const lo = chunk.code.items[ip.* + 1];
        ip.* += 2;

        return @as(u16, hi) << 8 | lo;
    }

    // Returns the current call frame
    fn currentFrame(self: *Vm) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }
};
