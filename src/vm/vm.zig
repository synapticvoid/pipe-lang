const std = @import("std");

const utils = @import("../utils.zig");
const types = @import("../types.zig");

const Caller = @import("value.zig").Caller;
const NativeFn = @import("value.zig").NativeFn;
const VmError = @import("value.zig").VmError;

const RuntimeContext = @import("../runtime.zig").RuntimeContext;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("value.zig").Value;
const FnObject = @import("program.zig").FnObject;
const Program = @import("program.zig").Program;

const MAX_STACK = 1024;
const MAX_FRAMES = 256;

const CallFrame = union(enum) {
    bytecode: Bytecode,
    native: Native,

    pub const Bytecode = struct {
        chunk: *const Chunk,
        ip: usize,
        base_slot: usize, // Where this frame's locals start on the value stack
        fn_index: ?u16, // Index of the current function. Used to check FnObject.result_name
    };

    pub const Native = struct {
        base_slot: usize, // Where this frame's locals start on the value stack
        function: NativeFn,
        arity: u8,
    };
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
        self.frames[0] = .{ .bytecode = .{
            .chunk = &self.program.chunk,
            .ip = 0,
            .base_slot = 0,
            .fn_index = null,
        } };
        self.frame_count = 1;
        return self.executeUntil(0);
    }

    pub fn caller(self: *Vm) Caller {
        return .{
            .ptr = self,
            .call_ptr = vmCallMethod,
            .resolve_method_ptr = vmResolveMethod,
        };
    }

    fn executeUntil(self: *Vm, target_depth: usize) !Value {
        // Hoist the frame pointer out of the loop; reload only on call/return when the active frame changes.
        var frame = &self.currentFrame().bytecode;

        while (self.frame_count > target_depth) {
            const op: OpCode = @enumFromInt(frame.chunk.code.items[frame.ip]);
            frame.ip += 1;

            switch (op) {
                // Constants and Literals
                .constant => {
                    const idx = readU16(frame.chunk, &frame.ip);
                    self.push(frame.chunk.constants.items[idx]);
                },
                .add, .subtract, .multiply, .divide => try self.executeBinaryArith(op),
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
                .equal => {
                    const a = self.stack[self.stack_top - 2];
                    const b = self.stack[self.stack_top - 1];

                    if (a == .struct_instance and b == .struct_instance) {
                        if (try self.findSpecialMethod(a.struct_instance.type_name, types.METHOD_EQUALS)) |method_idx| {
                            const fn_obj = &self.program.functions.items[method_idx];
                            const base_slot = self.stack_top - 2;
                            self.frames[self.frame_count] = .{ .bytecode = .{
                                .chunk = &fn_obj.chunk,
                                .ip = 0,
                                .base_slot = base_slot,
                                .fn_index = method_idx,
                            } };
                            self.frame_count += 1;
                            frame = &self.currentFrame().bytecode;
                            continue;
                        }

                        self.stack_top -= 2;
                        self.push(.{ .boolean = a.eql(b) });
                        continue;
                    }

                    _ = self.pop();
                    _ = self.pop();
                    self.push(.{ .boolean = a.eql(b) });
                },
                .greater, .less => {
                    const b = self.pop();
                    const a = self.pop();
                    std.debug.assert(a.isNumber() and b.isNumber());
                    self.push(.{ .boolean = switch (op) {
                        .greater => a.int > b.int,
                        .less => a.int < b.int,
                        else => unreachable,
                    } });
                },
                .true => self.push(.{ .boolean = true }),
                .false => self.push(.{ .boolean = false }),
                .null => self.push(.null),
                .unit => self.push(.unit),

                // Stack and Control Flow
                .pop => _ = self.pop(),
                .jump => {
                    frame.ip = readU16(frame.chunk, &frame.ip);
                },
                .jump_if_false => {
                    const offset = readU16(frame.chunk, &frame.ip);
                    const condition = self.pop();
                    if (!condition.isTruthy()) {
                        frame.ip = offset;
                    }
                },
                .loop => {
                    frame.ip = readU16(frame.chunk, &frame.ip);
                },
                .match_variant => try self.executeMatchVariant(frame),
                .@"return" => {
                    const ret_value = if (self.stack_top > frame.base_slot)
                        self.pop()
                    else
                        Value.unit;

                    self.stack_top = frame.base_slot;
                    self.frame_count -= 1;

                    if (self.frame_count == 0) {
                        return ret_value;
                    }

                    const final_value = if (frame.fn_index) |fn_index| blk: {
                        const function = self.program.functions.items[fn_index];
                        if (function.result_name) |name| {
                            break :blk try self.wrapResult(ret_value, name);
                        }
                        break :blk ret_value;
                    } else ret_value;

                    self.push(final_value);
                    frame = &self.currentFrame().bytecode;
                },

                // Variables
                .get_local => self.executeGetLocal(frame),
                .set_local => self.executeSetLocal(frame),
                .get_global => try self.executeGetGlobal(frame),
                .set_global => try self.executeSetGlobal(frame),
                .define_global => try self.executeDefineGlobal(frame),

                // Calls and Structs
                .call => {
                    const arity = readU8(frame.chunk, &frame.ip);
                    const base_slot = self.stack_top - 1 - arity;
                    const callee = self.stack[base_slot];
                    const args = self.stack[base_slot + 1 .. base_slot + 1 + arity];

                    const pushed_frame = try self.callValue(callee, args, base_slot, arity);
                    if (pushed_frame) {
                        frame = &self.currentFrame().bytecode;
                    }
                },
                .get_field => try self.executeGetField(frame),
                .set_field => try self.executeSetField(frame),
                .construct => try self.executeConstruct(frame),
            }
        }

        // Nested call, pop the result
        if (self.stack_top > 0) {
            return self.pop();
        }

        // Top-level call, just exit
        return Value.unit;
    }

    // NOTE: -- Opcode handlers

    inline fn executeBinaryArith(self: *Vm, op: OpCode) VmError!void {
        const b = self.pop();
        const a = self.pop();

        std.debug.assert(a.isNumber() and b.isNumber());

        self.push(.{ .int = switch (op) {
            .add => a.int + b.int,
            .subtract => a.int - b.int,
            .multiply => a.int * b.int,
            .divide => if (b.int == 0) return error.DivisionByZero else @divTrunc(a.int, b.int),
            else => unreachable,
        } });
    }

    inline fn executeMatchVariant(self: *Vm, frame: *CallFrame.Bytecode) VmError!void {
        const name_idx = readU16(frame.chunk, &frame.ip);
        const fail_jump = readU16(frame.chunk, &frame.ip);
        const variant_name = try getFieldNameFromConst(frame.chunk, name_idx);

        const top = self.stack[self.stack_top - 1];
        if (top == .struct_instance) {
            const type_name = top.struct_instance.type_name;
            if (utils.isVariant(type_name, variant_name)) {
                self.stack[self.stack_top - 1] = top.struct_instance.field_values[0];
            } else {
                frame.ip = fail_jump;
            }
        } else {
            frame.ip = fail_jump;
        }
    }

    inline fn executeGetLocal(self: *Vm, frame: *CallFrame.Bytecode) void {
        const idx = frame.base_slot + readU16(frame.chunk, &frame.ip);
        self.push(self.stack[idx]);
    }

    inline fn executeSetLocal(self: *Vm, frame: *CallFrame.Bytecode) void {
        const idx = frame.base_slot + readU16(frame.chunk, &frame.ip);
        self.stack[idx] = self.stack[self.stack_top - 1];
    }

    inline fn executeGetGlobal(self: *Vm, frame: *CallFrame.Bytecode) VmError!void {
        const idx = readU16(frame.chunk, &frame.ip);
        const val = self.globals.get(frame.chunk.constants.items[idx].string) orelse return error.UndefinedVariable;
        self.push(val);
    }

    inline fn executeSetGlobal(self: *Vm, frame: *CallFrame.Bytecode) VmError!void {
        const idx = readU16(frame.chunk, &frame.ip);
        const name = frame.chunk.constants.items[idx].string;

        if (self.globals.getPtr(name)) |ptr| {
            ptr.* = self.stack[self.stack_top - 1];
        } else {
            return error.UndefinedVariable;
        }
    }

    inline fn executeDefineGlobal(self: *Vm, frame: *CallFrame.Bytecode) VmError!void {
        const idx = readU16(frame.chunk, &frame.ip);
        const name = frame.chunk.constants.items[idx].string;

        const val = self.pop();
        try self.globals.put(self.allocator, name, val);
    }

    inline fn executeStructConstructor(self: *Vm, struct_def_idx: usize, arity: u8, base_slot: usize) VmError!void {
        const def = self.program.struct_defs.items[struct_def_idx];

        if (def.field_names.len != arity) {
            return error.ArityMismatch;
        }

        const arg0_idx = base_slot + 1;
        const args = self.stack[arg0_idx .. arg0_idx + def.field_names.len];
        var field_values = try self.allocator.alloc(Value, args.len);
        for (args, 0..) |arg, i| {
            field_values[i] = arg;
        }

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

        self.stack_top = base_slot;
        self.push(Value{ .struct_instance = instance });
    }

    inline fn executeGetField(self: *Vm, frame: *CallFrame.Bytecode) VmError!void {
        const field_idx = readU16(frame.chunk, &frame.ip);
        const field_name = try getFieldNameFromConst(frame.chunk, field_idx);

        const instance = switch (self.pop()) {
            .struct_instance => |si| si,
            else => return error.TypeError,
        };

        const field_meta = findField(instance, field_name);
        if (field_meta) |meta| {
            const field_value = if (meta.is_body)
                instance.body_field_values[meta.index]
            else
                instance.field_values[meta.index];

            self.push(field_value);
        } else {
            if (try self.resolveMethodInternal(instance, field_name)) |bound_method| {
                self.push(bound_method);
            } else {
                return error.UndefinedField;
            }
        }
    }

    inline fn executeSetField(self: *Vm, frame: *CallFrame.Bytecode) VmError!void {
        const field_idx = readU16(frame.chunk, &frame.ip);
        const field_name = try getFieldNameFromConst(frame.chunk, field_idx);

        const value = self.pop();

        const instance = switch (self.pop()) {
            .struct_instance => |si| si,
            else => return error.TypeError,
        };

        const field_meta = findField(instance, field_name) orelse return error.UndefinedField;
        if (field_meta.is_body) {
            instance.body_field_values[field_meta.index] = value;
        } else {
            instance.field_values[field_meta.index] = value;
        }

        self.push(value);
    }

    inline fn executeConstruct(self: *Vm, frame: *CallFrame.Bytecode) VmError!void {
        const struct_def_idx = readU16(frame.chunk, &frame.ip);
        const def = self.program.struct_defs.items[struct_def_idx];

        const field_values = try self.allocator.alloc(Value, def.field_names.len);
        var i = def.field_names.len;
        while (i > 0) {
            i -= 1;
            field_values[i] = self.pop();
        }

        const body_field_values = try self.allocator.alloc(Value, def.body_field_names.len);
        i = def.body_field_names.len;
        while (i > 0) {
            i -= 1;
            body_field_values[i] = Value.unit;
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
        self.push(.{ .struct_instance = instance });
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
    // A field can be:
    // - a struct field
    // - a struct body field
    fn findField(si: *Value.StructInstance, name: []const u8) ?struct { is_body: bool, index: usize } {
        // Search through struct fields
        for (si.field_names, 0..) |field_name, i| {
            if (std.mem.eql(u8, field_name, name)) {
                return .{ .is_body = false, .index = i };
            }
        }

        // Search through body fields
        for (si.body_field_names, 0..) |body_name, i| {
            if (std.mem.eql(u8, body_name, name)) {
                return .{ .is_body = true, .index = i };
            }
        }

        return null;
    }

    // Look up special method index from the global registry (equals(), hash(), etc.)
    // Returns fn_idx if found, null otherwise
    fn findSpecialMethod(self: *Vm, type_name: []const u8, method_name: []const u8) !?u16 {
        const qualified = try utils.memberName(self.allocator, type_name, method_name);
        defer self.allocator.free(qualified);

        const symbol = self.globals.get(qualified) orelse return null;
        if (symbol != .function) {
            return null;
        }

        return symbol.function;
    }

    // Returns the current call frame
    fn currentFrame(self: *Vm) *CallFrame {
        return &self.frames[self.frame_count - 1];
    }

    // Wrap the return value into an Ok/Err struct
    fn wrapResult(self: *Vm, ret_value: Value, result_name: []const u8) VmError!Value {
        // Don't double-wrap an already-wrapped Result (e.g. propagated Err from try)
        if (ret_value == .struct_instance and std.mem.startsWith(u8, ret_value.struct_instance.type_name, result_name)) {
            return ret_value;
        }

        const bang_idx = std.mem.indexOf(u8, result_name, "!") orelse result_name.len;
        const err_prefix = result_name[0..bang_idx];

        // Raw return value of the fn is a struct that starts with our E prefix
        if (ret_value == .struct_instance and std.mem.startsWith(u8, ret_value.struct_instance.type_name, err_prefix)) {
            // Wrap in an Err struct
            const type_name = try utils.memberName(self.allocator, result_name, types.RESULT_ERR_VARIANT);
            const values = try self.allocator.alloc(Value, 1);
            values[0] = ret_value;

            const field_names = try self.allocator.alloc([]const u8, 1);
            field_names[0] = "err";

            const instance = try self.allocator.create(Value.StructInstance);
            instance.* = .{
                .type_name = type_name,
                .field_names = field_names,
                .field_values = values,
                .body_field_names = &.{},
                .body_field_values = &.{},
                .kind = .case,
            };

            return Value{ .struct_instance = instance };
        }

        // If an Ok, wrap it and return the instance
        const type_name = try utils.memberName(self.allocator, result_name, types.RESULT_OK_VARIANT);
        const values = try self.allocator.alloc(Value, 1);
        values[0] = ret_value;

        const field_names = try self.allocator.alloc([]const u8, 1);
        field_names[0] = types.FIELD_VALUE;

        const instance = try self.allocator.create(Value.StructInstance);
        instance.* = .{
            .type_name = type_name,
            .field_names = field_names,
            .field_values = values,
            .body_field_names = &.{},
            .body_field_values = &.{},
            .kind = .case,
        };

        return Value{ .struct_instance = instance };
    }

    // Calls a VM function. Implementation of Caller struct
    fn vmCallMethod(ptr: *anyopaque, callable: Value, args: []const Value) VmError!Value {
        const self: *Vm = @ptrCast(@alignCast(ptr));
        return self.callInternal(callable, args);
    }

    fn callInternal(self: *Vm, callable: Value, args: []const Value) VmError!Value {
        // Push callable and args
        self.push(callable);
        for (args) |arg| {
            self.push(arg);
        }

        // Save it to known when the call returns
        // We might have nested calls!
        const saved_depth = self.frame_count;
        const arity: u8 = @intCast(args.len);
        const base_slot = self.stack_top - 1 - arity;

        // Prepare frames and stack
        if (try self.callValue(callable, args, base_slot, arity)) {
            return self.executeUntil(saved_depth);
        }

        // Result already on the stack
        return self.pop();
    }

    // Tries to return the bound method for a given struct instance and method name
    // Implementation of Caller struct.
    fn vmResolveMethod(ptr: *anyopaque, instance: *Value.StructInstance, method_name: []const u8) !?Value {
        const self: *Vm = @ptrCast(@alignCast(ptr));
        return self.resolveMethodInternal(instance, method_name);
    }

    fn resolveMethodInternal(self: *Vm, instance: *Value.StructInstance, name: []const u8) !?Value {
        const qualified = try utils.memberName(self.allocator, instance.type_name, name);
        defer self.allocator.free(qualified);

        const val = self.globals.get(qualified) orelse return null;
        if (val != .function) {
            return null;
        }

        return .{ .bound_method = .{
            .fn_idx = val.function,
            .receiver = instance,
        } };
    }

    fn callValue(
        self: *Vm,
        callable: Value,
        args: []const Value,
        base_slot: usize,
        arity: u8,
    ) VmError!bool {
        switch (callable) {
            .function => |fn_idx| {
                const fn_obj: *const FnObject = &self.program.functions.items[fn_idx];
                if (fn_obj.arity != arity) {
                    return error.ArityMismatch;
                }

                self.frames[self.frame_count] = .{ .bytecode = .{
                    .chunk = &fn_obj.chunk,
                    .ip = 0,
                    .base_slot = base_slot,
                    .fn_index = fn_idx,
                } };
                self.frame_count += 1;
            },
            .native => |native| {
                const ret_value = try native.func(args, self.ctx, self.caller());
                self.stack_top = base_slot;
                self.push(ret_value);
                return false;
            },
            .struct_constructor => |sc| try self.executeStructConstructor(sc, arity, base_slot),
            .bound_method => |bm| {
                const fn_obj: *const FnObject = &self.program.functions.items[bm.fn_idx];
                if (fn_obj.arity - 1 != arity) {
                    return error.ArityMismatch;
                }
                self.stack[base_slot] = .{ .struct_instance = bm.receiver };
                self.frames[self.frame_count] = .{ .bytecode = .{
                    .chunk = &fn_obj.chunk,
                    .ip = 0,
                    .base_slot = base_slot,
                    .fn_index = bm.fn_idx,
                } };
                self.frame_count += 1;
            },
            else => return error.NotCallable,
        }

        return true;
    }
};
