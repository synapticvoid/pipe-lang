pub const OpCode = enum(u8) {
    // =========================================================================
    // Constants and Literals
    // =========================================================================

    // Encoding: constant u16
    // Operands: const_idx: u16 (constant pool index)
    // Stack: [...] -> [..., constant]
    constant,

    // Arithmetic
    // Operands: none
    // Stack (binary): [..., left, right] -> [..., result]
    // Notes: pops right then left, pushes computed result.
    add,
    subtract,
    multiply,
    divide,

    // Encoding: negate
    // Operands: none
    // Stack: [..., value] -> [..., -value]
    negate,

    // Logic
    // Encoding: not
    // Operands: none
    // Stack: [..., value] -> [..., boolean]
    // Notes: pops value, pushes !truthy(value).
    not,

    // Comparison
    // Operands: none
    // Stack: [..., left, right] -> [..., boolean]
    equal,
    greater,
    less,

    // Constant values
    // Operands: none
    // Stack: [...] -> [..., literal]
    true,
    false,
    null,
    unit,

    // =========================================================================
    // Stack and Control Flow
    // =========================================================================

    // Stack operations
    // Encoding: pop
    // Operands: none
    // Stack: [..., value] -> [...]
    pop,

    // Control flow
    // Encoding: jump u16
    // Operands: target_ip: u16 (absolute instruction pointer)
    // Stack: unchanged
    jump,

    // Encoding: jump_if_false u16
    // Operands: target_ip: u16 (absolute instruction pointer)
    // Stack: [..., condition] -> [...]
    // Notes: pops condition; jumps when condition is falsy.
    jump_if_false,

    // Encoding: loop u16
    // Operands: target_ip: u16 (absolute instruction pointer)
    // Stack: unchanged
    loop,

    // Encoding: return
    // Operands: none
    // Stack: [..., value] -> caller frame receives value
    @"return",

    // =========================================================================
    // Variables
    // =========================================================================

    // Local variables
    // Encoding: get_local u16
    // Operands: slot_idx: u16 (frame-relative stack slot)
    // Stack: [...] -> [..., local_value]
    get_local,

    // Encoding: set_local u16
    // Operands: slot_idx: u16 (frame-relative stack slot)
    // Stack: [..., value] -> [..., value]
    // Notes: writes top-of-stack value to slot, does not pop it.
    set_local,

    // Global variables
    // Encoding: get_global u16
    // Operands: name_const_idx: u16 (constant pool string)
    // Stack: [...] -> [..., global_value]
    get_global,

    // Encoding: set_global u16
    // Operands: name_const_idx: u16 (constant pool string)
    // Stack: [..., value] -> [..., value]
    // Notes: writes top-of-stack value to existing global, does not pop it.
    set_global,

    // Encoding: define_global u16
    // Operands: name_const_idx: u16 (constant pool string)
    // Stack: [..., value] -> [...]
    // Notes: pops value and defines global by name.
    define_global,

    // =========================================================================
    // Calls and Structs
    // =========================================================================

    // Function call
    // Encoding: call u8
    // Operands: arity: u8
    // Stack: [..., callee, arg1, ..., argN] -> [..., return_value]
    call,

    // Structs
    // Encoding: get_field u16
    // Operands: field_name_const_idx: u16 (constant pool string)
    // Stack: [..., instance] -> [..., field_value]
    // Notes: pops instance, looks up field by name, pushes field value.
    get_field,

    // Encoding: set_field u16
    // Operands: field_name_const_idx: u16 (constant pool string)
    // Stack: [..., instance, field_value] -> [..., field_value]
    // Notes: pops value then instance, mutates field, pushes assigned value.
    set_field,

    // Encoding: construct u16
    // Operands: struct_def_idx: u16 (Program struct definition table index)
    // Stack: [..., arg1, ..., argN] -> [..., instance]
    // Notes: pops N args using struct metadata, creates StructInstance, pushes it.
    construct,
};
