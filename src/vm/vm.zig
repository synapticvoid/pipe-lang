const std = @import("std");
const RuntimeContext = @import("../runtime.zig").RuntimeContext;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("value.zig").Value;
const FnObject = @import("program.zig").FnObject;
const Program = @import("program.zig").Program;

pub const VmError = error{
    DivisionByZero,
    TypeError, // wrong types for arithmetic/negate/table
    UndefinedVariable,
    UndefinedField,
    ArityMismatch,
    NotCallable, // Not a function/method that we can call
};

const MAX_STACK = 256;
const MAX_FRAMES = 64;

const CallFrame = struct {
    chunk: *const Chunk,
    ip: usize,
    base_slot: usize, // Where this frame's locals start on the value stack
};

pub const Vm = struct {
    // Program containing the Chunk + VM tables
    program: *const Program,

    // Use Arrays rather than ArrayLists for performance.
    // The size is fixed anyway, so we don't need to worry about resizing.
    // ArrayList data is contiguous but lives on the heap behind a pointer,
    // meaning that each access requires a pointer indirection.
    // With an Array, the value is just an arithmetic offset off the struct base pointer.

    // Value stack
    stack: [MAX_STACK]Value,
    stack_top: usize,

    // Call frame stack
    frames: [MAX_FRAMES]CallFrame,
    frame_count: usize = 0,

    // Global variables table
    // name -> value
    globals: std.StringHashMapUnmanaged(Value),

    ctx: RuntimeContext,

    allocator: std.mem.Allocator,

    pub fn init(program: *const Program, ctx: RuntimeContext, allocator: std.mem.Allocator) Vm {
        return .{
            .program = program,
            .stack = undefined,
            .stack_top = 0,
            .frames = undefined,
            .frame_count = 0,
            .globals = .{},
            .ctx = ctx,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Vm) void {
        self.globals.deinit(self.allocator);
    }

    pub fn run(self: *Vm) !Value {
        // Setup initial frame for top-level
        self.frames[0] = .{ .chunk = &self.program.chunk, .ip = 0, .base_slot = 0 };
        self.frame_count = 1;
        return self.execute();
    }

    fn execute(self: *Vm) !Value {
        // Hoist the frame pointer out of the loop; reload only on call/return when the active frame changes.
        var frame = self.currentFrame();

        while (self.frame_count > 0) {
            const op: OpCode = @enumFromInt(frame.chunk.code.items[frame.ip]);
            frame.ip += 1;

            switch (op) {
                .constant => {
                    // Read pool index from chunk
                    const idx = readU16(frame.chunk, &frame.ip);
                    self.push(frame.chunk.constants.items[idx]);
                },
                .true => self.push(.{ .boolean = true }),
                .false => self.push(.{ .boolean = false }),
                .null => self.push(.null),
                .unit => self.push(.unit),
                .pop => _ = self.pop(),
                .negate => {
                    const val = self.pop();
                    if (!val.isNumber()) {
                        return VmError.TypeError;
                    }

                    self.push(switch (val) {
                        .int => |n| .{ .int = -n },
                        else => unreachable,
                    });
                },
                .not => {
                    const val = self.pop();
                    self.push(.{ .boolean = !val.isTruthy() });
                },
                .add, .subtract, .multiply, .divide => {
                    const b = self.pop(); // right
                    const a = self.pop(); // left

                    std.debug.assert(a.isNumber() and b.isNumber());

                    self.push(.{ .int = switch (op) {
                        .add => a.int + b.int,
                        .subtract => a.int - b.int,
                        .multiply => a.int * b.int,
                        .divide => if (b.int == 0) return error.DivisionByZero else @divTrunc(a.int, b.int),
                        else => unreachable,
                    } });
                },
                .equal => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(.{ .boolean = a.eql(b) });
                },
                .greater, .less => {
                    // Read operands from stack
                    const b = self.pop();
                    const a = self.pop();

                    std.debug.assert(a.isNumber() and b.isNumber());

                    // Compare them and push the result
                    self.push(.{ .boolean = switch (op) {
                        .greater => a.int > b.int,
                        .less => a.int < b.int,
                        else => unreachable,
                    } });
                },
                .define_global => {
                    // Read pool index to retrieve the name
                    const idx = readU16(frame.chunk, &frame.ip);
                    const name = frame.chunk.constants.items[idx].string;

                    const val = self.pop();
                    try self.globals.put(self.allocator, name, val);
                },
                .get_global => {
                    const idx = readU16(frame.chunk, &frame.ip);
                    const val = self.globals.get(frame.chunk.constants.items[idx].string) orelse return error.UndefinedVariable;
                    self.push(val);
                },
                .set_global => {
                    const idx = readU16(frame.chunk, &frame.ip);
                    const name = frame.chunk.constants.items[idx].string;

                    if (self.globals.getPtr(name)) |ptr| {
                        ptr.* = self.stack[self.stack_top - 1];
                    } else {
                        return error.UndefinedVariable;
                    }
                },
                .get_local => {
                    const idx = frame.base_slot + readU16(frame.chunk, &frame.ip);
                    self.push(self.stack[idx]);
                },
                .set_local => {
                    const idx = frame.base_slot + readU16(frame.chunk, &frame.ip);
                    self.stack[idx] = self.stack[self.stack_top - 1];
                },
                .jump => frame.ip = readU16(frame.chunk, &frame.ip),
                .jump_if_false => {
                    // Read offset
                    const offset = readU16(frame.chunk, &frame.ip);

                    // Evalute condition on stack
                    const condition = self.pop();
                    if (!condition.isTruthy()) {
                        frame.ip = offset;
                    }
                },
                .loop => frame.ip = readU16(frame.chunk, &frame.ip),
                .@"return" => {
                    // Pop the return value (or unit if stack is at base)
                    const ret_value = if (self.stack_top > frame.base_slot)
                        self.pop()
                    else
                        Value.unit;

                    // Remove all locals of the current stack frame
                    self.stack_top = frame.base_slot;
                    self.frame_count -= 1;

                    // Top-level frame, return the value
                    if (self.frame_count == 0) {
                        return ret_value;
                    }

                    // Push the return value on the stack of the caller
                    self.push(ret_value);
                    // Update the frame only if we are NOT at the top level
                    frame = self.currentFrame();
                },
                .call => {
                    // Read function arity
                    const arity = readU8(frame.chunk, &frame.ip);

                    // Peak the callee to check if we can cast it to *FnObject
                    const base_slot = self.stack_top - 1 - arity;
                    const callee = self.stack[base_slot];

                    switch (callee) {
                        .function => {
                            const fn_obj: *const FnObject = &self.program.functions.items[callee.function];

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
                            frame = self.currentFrame();
                        },
                        .native => {
                            // Slice of the fn args
                            const arg0_idx = base_slot + 1;
                            const args = self.stack[arg0_idx .. arg0_idx + arity];

                            // Call builtin function, remove the args and push the return value
                            const ret_value = callee.native.func(args, self.ctx);
                            self.stack_top = base_slot;
                            self.push(ret_value);
                        },
                        .struct_constructor => {
                            // Stack: [ ..., constructor_value, arg0, arg1, ... ]
                            //                    ^ base_slot
                            const struct_def_idx = callee.struct_constructor;
                            const def = self.program.struct_defs.items[struct_def_idx];

                            // Check arity
                            if (def.field_names.len != arity) {
                                return error.ArityMismatch;
                            }

                            // Allocate args
                            const arg0_idx = base_slot + 1;
                            const args = self.stack[arg0_idx .. arg0_idx + def.field_names.len];
                            var field_values = try self.allocator.alloc(Value, args.len);
                            for (args, 0..) |arg, i| {
                                field_values[i] = arg;
                            }

                            // Allocate body args
                            const body_field_values = try self.allocator.alloc(Value, def.body_field_names.len);
                            for (body_field_values) |*v| {
                                v.* = Value.unit;
                            }

                            const instance = try self.allocator.create(Value.StructInstance);
                            instance.* = .{
                                .type_name = def.name,
                                .field_names = def.field_names,
                                .body_field_names = def.body_field_names,
                                .field_values = field_values,
                                .body_field_values = body_field_values,
                                .kind = def.kind,
                            };

                            // Reset stack top now that we consumed everything
                            self.stack_top = base_slot;
                            self.push(Value{ .struct_instance = instance });
                        },
                        else => return error.NotCallable,
                    }
                },
                .get_field => {
                    // Get field metadata
                    const field_idx = readU16(frame.chunk, &frame.ip);
                    const field_name = try getFieldNameFromConst(frame.chunk, field_idx);

                    // Get instance
                    const instance = switch (self.pop()) {
                        .struct_instance => |si| si,
                        else => return error.TypeError,
                    };

                    // Get field value
                    const field_meta = findField(instance, field_name) orelse return error.UndefinedField;
                    const field_value = if (field_meta.is_body)
                        instance.body_field_values[field_meta.index]
                    else
                        instance.field_values[field_meta.index];

                    self.push(field_value);
                },
                .set_field => {
                    // Get struct
                    const field_idx = readU16(frame.chunk, &frame.ip);
                    const field_name = try getFieldNameFromConst(frame.chunk, field_idx);

                    // Pop value
                    const value = self.pop();

                    // Pop instance
                    const instance = switch (self.pop()) {
                        .struct_instance => |si| si,
                        else => return error.TypeError,
                    };

                    const field_meta = findField(instance, field_name) orelse return error.UndefinedField;
                    if (field_meta.is_body)
                        instance.body_field_values[field_meta.index] = value
                    else
                        instance.field_values[field_meta.index] = value;

                    self.push(value);
                },
                .construct => {
                    const struct_def_idx = readU16(frame.chunk, &frame.ip);
                    const def = self.program.struct_defs.items[struct_def_idx];

                    // Allocate field values
                    const field_values = try self.allocator.alloc(Value, def.field_names.len);
                    var i = def.field_names.len;
                    while (i > 0) {
                        i -= 1;
                        field_values[i] = self.pop();
                    }

                    // Allocate body field values
                    const body_field_values = try self.allocator.alloc(Value, def.body_field_names.len);
                    i = def.body_field_names.len;
                    while (i > 0) {
                        i -= 1;
                        body_field_values[i] = Value.unit;
                    }

                    // Create a push field instance
                    const instance = try self.allocator.create(Value.StructInstance);
                    instance.* = .{
                        .type_name = def.name,
                        .field_names = def.field_names,
                        .body_field_names = def.body_field_names,
                        .field_values = field_values,
                        .body_field_values = body_field_values,
                        .kind = def.kind,
                    };
                    self.push(.{ .struct_instance = instance });
                },
            }
        }

        return .unit;
    }

    // NOTE: -- Helpers

    // Pushes the value onto the stack
    fn push(self: *Vm, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    // Pops the value from the stack
    fn pop(self: *Vm) Value {
        std.debug.assert(self.stack_top > 0);
        self.stack_top -= 1;
        return self.stack[self.stack_top];
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

    // Returns the field name from the constant pool
    fn getFieldNameFromConst(chunk: *const Chunk, idx: u16) VmError![]const u8 {
        const name = chunk.constants.items[idx];
        return switch (name) {
            .string => |s| s,
            else => error.UndefinedField,
        };
    }

    // Returns the field's metadata from its name
    fn findField(si: *Value.StructInstance, name: []const u8) ?struct { is_body: bool, index: usize } {
        for (si.field_names, 0..) |field_name, i| {
            if (std.mem.eql(u8, field_name, name)) {
                return .{ .is_body = false, .index = i };
            }
        }

        for (si.body_field_names, 0..) |body_name, i| {
            if (std.mem.eql(u8, body_name, name)) {
                return .{ .is_body = true, .index = i };
            }
        }

        return null;
    }

    // Returns the current call frame
    fn currentFrame(self: *Vm) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }
};
