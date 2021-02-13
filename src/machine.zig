const std = @import("std");

const heap = std.heap;
const mem = std.mem;

usingnamespace @import("state.zig");

pub const Machine = struct {
    allocator: *mem.Allocator,

    chunk: *Chunk,

    pub fn init(allocator: *mem.Allocator, chunk: *Chunk) Machine {
        return .{
            .allocator = allocator,
            .chunk = chunk,
        };
    }

    pub fn deinit(self: *Machine) void {}

    pub fn step(self: *Machine) !void {
        
    }
};
