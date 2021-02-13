const std = @import("std");

const state = @import("state.zig");

comptime {
    std.testing.refAllDecls(@This());
}

pub fn main() !void {
    
}
