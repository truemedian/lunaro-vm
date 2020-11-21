const std = @import("std");

const vm = @import("vm.zig");

comptime {
    std.testing.refAllDecls(@This());
}

pub fn main() !void {
    try vm.run();
}
