pub const OpCode = enum(u8) {
    // Constants (u16 operand: pool index)
    constant,

    // Arithmetic
    add,
    subtract,
    multiply,
    divide,
    negate,

    // Logic
    not,
    equal,
    greater,
    less,

    // Constant values
    true,
    false,
    null,
    unit,

    // Stack operations
    pop, // Discard top of the stack

    // Control flow (u16 operand: jump offset)
    jump,
    jump_if_false,
    loop,
    @"return",

    // Local variables (u16 operand: stack slot index)
    get_local,
    set_local,

    // Global variables (u16 operand: constant index for name)
    get_global,
    set_global,
    define_global,

    // Function (u8 operand: arity)
    call,
};
