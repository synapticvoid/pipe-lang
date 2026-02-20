pub const PipeType = enum {
    int,
    float,
    bool,
    string,
    unit,
    any,

    pub fn isNumeric(self: PipeType) bool {
        return self == .int or self == .float;
    }

    pub fn compatible(self: PipeType, other: PipeType) bool {
        if (self == other) {
            return true;
        }

        if (self == .any or other == .any) {
            return true;
        }

        return false;
    }
};
