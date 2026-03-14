const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("opcode.zig").OpCode;
const Value = @import("../ast.zig").Value;

pub const VmError = error{
    StackUnderflow, // Pop from empty stack
    TypeError, // wront types for arithmetic/negate/table
    DivisionByZero,
    UndefinedVariable,
};

pub const Vm = struct {
    // Value stack
    stack: std.ArrayList(Value),

    // Global variables table
    // name -> value
    globals: std.StringHashMapUnmanaged(Value),

    // for the print opcode
    output_printer: ?*std.Io.Writer,

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) Vm {
        return .{
            .stack = .{},
            .globals = .{},
            .output_printer = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Vm) void {
        self.stack.deinit(self.allocator);
        self.globals.deinit(self.allocator);
    }

    pub fn setOutputWriter(self: *Vm, writer: *std.Io.Writer) void {
        self.output_printer = writer;
    }

    pub fn run(self: *Vm, chunk: *const Chunk) !Value {
        var ip: usize = 0;
        while (ip < chunk.code.items.len) {
            const op: OpCode = @enumFromInt(chunk.code.items[ip]);
            ip += 1;

            switch (op) {
                .constant => {
                    // Read pool index from chunk
                    const idx = readU16(chunk, &ip);
                    try self.push(chunk.constants.items[idx]);
                },
                .true => try self.push(.{ .boolean = true }),
                .false => try self.push(.{ .boolean = false }),
                .null => try self.push(.null),
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
                    const idx = readU16(chunk, &ip);
                    const name = chunk.constants.items[idx].string;

                    const val = try self.pop();
                    try self.globals.put(self.allocator, name, val);
                },
                .get_global => {
                    const idx = readU16(chunk, &ip);
                    const val = self.globals.get(chunk.constants.items[idx].string) orelse return error.UndefinedVariable;
                    try self.push(val);
                },
                .set_global => {
                    const idx = readU16(chunk, &ip);
                    const name = chunk.constants.items[idx].string;

                    if (self.globals.getPtr(name)) |ptr| {
                        ptr.* = self.stack.getLast();
                    } else {
                        return error.UndefinedVariable;
                    }
                },
                .get_local => {
                    const idx = readU16(chunk, &ip);
                    try self.push(self.stack.items[idx]);
                },
                .set_local => {
                    const idx = readU16(chunk, &ip);
                    self.stack.items[idx] = self.stack.getLast();
                },
                .jump => ip = readU16(chunk, &ip),
                .jump_if_false => {
                    // Read offset
                    const offset = readU16(chunk, &ip);

                    // Evalute condition on stack
                    const condition = try self.pop();
                    if (!condition.isTruthy()) {
                        ip = offset;
                    }
                },
                .loop => ip = readU16(chunk, &ip),
                .print => {
                    const val = try self.pop();
                    if (self.output_printer) |writer| {
                        try val.format(writer);
                        try writer.writeByte('\n');
                    }
                },
                .@"return" => {
                    if (self.stack.items.len == 0) return .unit;
                    return try self.pop();
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

    // Reas a 16-bit value from the code
    // and advances the instruction pointer
    fn readU16(chunk: *const Chunk, ip: *usize) u16 {
        // Decode high and low bytes
        const hi = chunk.code.items[ip.*];
        const lo = chunk.code.items[ip.* + 1];
        ip.* += 2;

        return @as(u16, hi) << 8 | lo;
    }
};
