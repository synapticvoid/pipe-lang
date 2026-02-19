pub const PipeType = enum {
    int,
    float,
    bool,
    string,
    unit,

    pub fn isNumeric(self: PipeType) bool {
        return self == .int or self == .float;
    }
};
