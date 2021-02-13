const std = @import("std");

const debug = std.debug;
const math = std.math;
const mem = std.mem;
const fmt = std.fmt;
const io = std.io;

const Endian = std.builtin.Endian;

const float_t = f64;
const int_t = u64;
const size_t = usize;

const FixedBufferReader = io.FixedBufferStream([]const u8).Reader;

comptime {
    debug.assert(@bitSizeOf(float_t) == @bitSizeOf(int_t));
}

pub const State = struct {
    version: u8,
    endian: Endian,
    sizeof_int: u8,
    sizeof_size: u8,
    sizeof_instruction: u8,
    sizeof_number: u8,
    number_type: NumberFormat,

    pub fn format(self: State, comptime str: []const u8, options: fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Lua {x}, {s} Endian, {d}-bit Integers, {d}-bit Sizes, {d}-bit Instructions, {d}-bit {s} Numbers", .{
            self.version,           @tagName(self.endian),      self.sizeof_int * 8, self.sizeof_size * 8, self.sizeof_instruction * 8,
            self.sizeof_number * 8, @tagName(self.number_type),
        });
    }

    pub fn init(header: []const u8) !State {
        if (header.len < 12) return error.InvalidHeader;
        if (!mem.eql(u8, header[0..4], "\x1BLua")) return error.InvalidBytecode;

        const version_byte = header[4];
        const format_byte = header[5];
        const endian_byte = header[6];
        const sizeof_int_byte = header[7];
        const sizeof_size_byte = header[8];
        const sizeof_instruction_byte = header[9];
        const sizeof_number_byte = header[10];
        const number_type_byte = header[11];

        if (format_byte != 0x00) return error.InvalidFormat;

        const endian = switch (endian_byte) {
            0x00 => Endian.Big,
            0x01 => Endian.Little,
            else => return error.InvalidEndian,
        };

        if (sizeof_int_byte > @sizeOf(size_t)) return error.IntegerTooLarge;
        if (sizeof_size_byte > @sizeOf(size_t)) return error.SizeTooLarge;
        if (sizeof_instruction_byte != 4) return error.UnknownInstructionSize;
        if (sizeof_number_byte > @sizeOf(size_t)) return error.NumberTooLarge;

        const number_type = switch (number_type_byte) {
            0x00 => NumberFormat.floating,
            0x01 => NumberFormat.integer,
            else => return error.InvalidNumberType,
        };

        return State{
            .version = version_byte,
            .endian = endian,
            .sizeof_int = sizeof_int_byte,
            .sizeof_size = sizeof_size_byte,
            .sizeof_instruction = sizeof_instruction_byte,
            .sizeof_number = sizeof_number_byte,
            .number_type = number_type,
        };
    }

    pub fn readInteger(self: State, reader: FixedBufferReader) !int_t {
        var buffer = mem.zeroes([@divExact(@typeInfo(int_t).Int.bits, 8)]u8);

        switch (self.endian) {
            .Big => {
                _ = try reader.readAll(buffer[buffer.len - self.sizeof_int ..]);

                return mem.readIntBig(int_t, &buffer);
            },
            .Little => {
                _ = try reader.readAll(buffer[0..self.sizeof_int]);

                return mem.readIntLittle(int_t, &buffer);
            },
        }
    }

    pub fn readSize(self: State, reader: FixedBufferReader) !size_t {
        var buffer = mem.zeroes([@divExact(@typeInfo(size_t).Int.bits, 8)]u8);

        switch (self.endian) {
            .Big => {
                _ = try reader.readAll(buffer[buffer.len - self.sizeof_size ..]);

                return mem.readIntBig(size_t, &buffer);
            },
            .Little => {
                _ = try reader.readAll(buffer[0..self.sizeof_size]);

                return mem.readIntLittle(size_t, &buffer);
            },
        }
    }

    pub fn readString(self: State, allocator: *mem.Allocator, reader: FixedBufferReader) ![:0]const u8 {
        const size = try self.readSize(reader);

        var buffer = try allocator.allocSentinel(u8, size, 0);
        _ = try reader.readAll(buffer);

        return buffer;
    }

    pub fn readNumber(self: State, reader: FixedBufferReader) !LuaNumber {
        var buffer = mem.zeroes([@divExact(@typeInfo(int_t).Int.bits, 8)]u8);

        switch (self.endian) {
            .Big => {
                _ = try reader.readAll(buffer[buffer.len - self.sizeof_number ..]);

                const int = mem.readIntBig(int_t, &buffer);

                switch (self.number_type) {
                    .integer => return LuaNumber{
                        .integer = int,
                    },
                    .floating => return LuaNumber{
                        .floating = @bitCast(float_t, int),
                    },
                }
            },
            .Little => {
                _ = try reader.readAll(buffer[0..self.sizeof_number]);

                const int = mem.readIntLittle(int_t, &buffer);

                switch (self.number_type) {
                    .integer => return LuaNumber{
                        .integer = int,
                    },
                    .floating => return LuaNumber{
                        .floating = @bitCast(float_t, int),
                    },
                }
            },
        }
    }
};

pub const NumberFormat = std.meta.TagType(LuaNumber);
pub const LuaNumber = union(enum) {
    floating: float_t,
    integer: int_t,
};

pub const String = [:0]const u8;

pub const Instruction = union(enum) {
    pub const Opcode = enum(u6) {
        MOVE = 0,
        LOADK = 1,
        LOADBOOL = 2,
        LOADNIL = 3,
        GETUPVAL = 4,
        GETGLOBAL = 5,
        GETTABLE = 6,
        SETGLOBAL = 7,
        SETUPVAL = 8,
        SETTABLE = 9,
        NEWTABLE = 10,
        SELF = 11,
        ADD = 12,
        SUB = 13,
        MUL = 14,
        DIV = 15,
        MOD = 16,
        POW = 17,
        UNM = 18,
        NOT = 19,
        LEN = 20,
        CONCAT = 21,
        JMP = 22,
        EQ = 23,
        LT = 24,
        LE = 25,
        TEST = 26,
        TESTSET = 27,
        CALL = 28,
        TAILCALL = 29,
        RETURN = 30,
        FORLOOP = 31,
        FORPREP = 32,
        TFORLOOP = 33,
        SETLIST = 34,
        CLOSE = 35,
        CLOSURE = 36,
        VARARG = 37,
    };

    pub const iABC = struct {
        opcode: Opcode,
        A: u8,
        B: u9,
        C: u9,
    };

    pub const iABx = struct {
        opcode: Opcode,
        A: u8,
        Bx: u18,
    };

    pub const iAsBx = struct {
        pub const Bias = -math.maxInt(i18);

        opcode: Opcode,
        A: u8,
        sBx: i19,
    };

    ABC: iABC,
    ABx: iABx,
    AsBx: iAsBx,

    pub fn format(self: Instruction, comptime str: []const u8, options: fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .ABC => |inst| {
                try writer.print("{s: <9} ", .{@tagName(inst.opcode)});

                switch (inst.opcode) {
                    .CLOSE => {
                        try writer.print("{d}", .{inst.A});
                    },

                    .MOVE, .LOADNIL, .GETUPVAL, .SETUPVAL, .UNM, .NOT, .LEN, .RETURN, .VARARG => {
                        try writer.print("{d}, {d}", .{ inst.A, inst.B });
                    },

                    .TEST, .TFORLOOP => {
                        try writer.print("{d}, {d}", .{ inst.A, inst.C });
                    },

                    .LOADBOOL, .GETTABLE, .SETTABLE, .NEWTABLE, .SELF, .ADD, .SUB, .MUL, .DIV, .MOD, .POW, .CONCAT, .EQ, .LT, .LE, .TESTSET, .CALL, .TAILCALL, .SETLIST => {
                        try writer.print("{d}, {d}, {d}", .{ inst.A, inst.B, inst.C });
                    },
                    else => unreachable,
                }
            },
            .ABx => |inst| {
                try writer.print("{s: <9} {d}, {d}", .{ @tagName(inst.opcode), inst.A, inst.Bx });
            },
            .AsBx => |inst| {
                try writer.print("{s: <9} {d}, {d}", .{ @tagName(inst.opcode), inst.A, inst.sBx });
            },
        }
    }

    pub fn decode(input: u32) Instruction {
        const opcode = @intToEnum(Opcode, @truncate(u6, input));

        switch (opcode) {
            .MOVE, .LOADBOOL, .LOADNIL, .GETUPVAL, .GETTABLE, .SETUPVAL, .SETTABLE, .NEWTABLE, .SELF, .ADD, .SUB, .MUL, .DIV, .MOD, .POW, .UNM, .NOT, .LEN, .CONCAT, .EQ, .LT, .LE, .TEST, .TESTSET, .CALL, .TAILCALL, .RETURN, .TFORLOOP, .SETLIST, .CLOSE, .VARARG => {
                return .{
                    .ABC = .{
                        .opcode = opcode,
                        .A = @truncate(u8, input >> 6),
                        .B = @truncate(u9, input >> 14),
                        .C = @truncate(u9, input >> 23),
                    },
                };
            },
            .LOADK, .GETGLOBAL, .SETGLOBAL, .CLOSURE => {
                return .{
                    .ABx = .{
                        .opcode = opcode,
                        .A = @truncate(u8, input >> 6),
                        .Bx = @truncate(u18, input >> 14),
                    },
                };
            },
            .JMP, .FORLOOP, .FORPREP => {
                return .{
                    .AsBx = .{
                        .opcode = opcode,
                        .A = @truncate(u8, input >> 6),
                        .sBx = @intCast(i18, @intCast(i19, @truncate(u18, input >> 14)) + iAsBx.Bias),
                    },
                };
            },
        }
    }
};

pub const Constant = union(enum) {
    nil: void,
    boolean: bool,
    number: LuaNumber,
    string: String,
};

pub const Chunk = struct {
    allocator: *mem.Allocator,
    state: State,

    name: String,
    first_line: int_t,
    last_line: int_t,
    n_upvalues: u8,
    n_arguments: u8,
    vararg: u8,
    stack_size: u8,

    instructions: []const Instruction,
    constants: []const Constant,
    prototypes: []const *Chunk,
    debug: DebugInfo,

    const DecodeError = mem.Allocator.Error || FixedBufferReader.Error || error{ InvalidConstantType, EndOfStream };

    pub fn format(self: Chunk, comptime str: []const u8, options: fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        try writer.print("\nChunk[{}]: {s} on lines {} to {}, {} upvalues, {} arguments, {b:0>3} vararg\n", .{
            self.stack_size, self.name, self.first_line, self.last_line, self.n_upvalues, self.n_arguments, self.vararg,
        });

        try writer.print("Constants[{}]: \n", .{self.constants.len});
        for (self.constants) |constant, i| {
            try writer.print("  {} ({s}): ", .{ i, @tagName(constant) });
            switch (constant) {
                .nil => try writer.writeAll("nil"),
                .boolean => |data| try writer.print("{}", .{data}),
                .number => |data| try writer.print("{}", .{data}),
                .string => |data| {
                    if (data.len > 32) {
                        try writer.print("\"{}...\"", .{
                            std.zig.fmtEscapes(data[0..32]),
                        });
                    } else {
                        try writer.print("\"{}\"", .{
                            std.zig.fmtEscapes(data[0 .. data.len - 1]),
                        });
                    }
                },
            }

            try writer.writeAll("\n");
        }

        try writer.print("Instructions[{}]: \n", .{self.instructions.len});
        for (self.instructions) |instruction| {
            try writer.print("  {}\n", .{instruction});
        }

        try writer.print("Prototypes[{}]: \n", .{self.prototypes.len});
        for (self.prototypes) |proto| {
            try proto.format(str, options, writer);
        }

        try writer.print("Source Lines[{}]: ", .{self.debug.lines.len});
        for (self.debug.lines) |line| {
            try writer.print("{}, ", .{line});
        }

        try writer.writeAll("\n");

        try writer.print("Locals[{}]: \n", .{self.debug.locals.len});
        for (self.debug.locals) |local, i| {
            try writer.print("  {}: '{s}' from {} to {}\n", .{ i, local.name, local.start_pc, local.end_pc });
        }

        try writer.print("Upvalues[{}]: \n", .{self.debug.upvalues.len});
        for (self.debug.upvalues) |upvalue, i| {
            try writer.print("  {}: '{s}'", .{ i, upvalue.name });
        }
    }

    pub fn decode(allocator: *mem.Allocator, state: State, reader: FixedBufferReader) DecodeError!*Chunk {
        var chunk = try allocator.create(Chunk);
        errdefer allocator.destroy(chunk);

        chunk.allocator = allocator;
        chunk.state = state;

        chunk.name = try state.readString(allocator, reader);
        errdefer allocator.free(chunk.name);

        chunk.first_line = try state.readInteger(reader);
        chunk.last_line = try state.readInteger(reader);

        chunk.n_upvalues = try reader.readInt(u8, state.endian);
        chunk.n_arguments = try reader.readInt(u8, state.endian);
        chunk.vararg = try reader.readInt(u8, state.endian);
        chunk.stack_size = try reader.readInt(u8, state.endian);

        {
            const list_size = try state.readInteger(reader);

            var instructions = try allocator.alloc(Instruction, list_size);

            var i: usize = 0;
            while (i < list_size) : (i += 1) {
                const instr_int = try reader.readInt(u32, state.endian);

                instructions[i] = Instruction.decode(instr_int);
            }

            chunk.instructions = instructions;
        }

        {
            const list_size = try state.readInteger(reader);

            var constants = try allocator.alloc(Constant, list_size);

            var i: usize = 0;
            while (i < list_size) : (i += 1) {
                const const_type = try reader.readInt(u8, state.endian);

                switch (const_type) {
                    0 => {
                        constants[i] = Constant.nil;
                    },
                    1 => {
                        constants[i] = .{
                            .boolean = (try reader.readInt(u8, state.endian)) != 0,
                        };
                    },
                    3 => {
                        constants[i] = .{
                            .number = try state.readNumber(reader),
                        };
                    },
                    4 => {
                        constants[i] = .{
                            .string = try state.readString(allocator, reader),
                        };
                    },
                    else => return error.InvalidConstantType,
                }
            }

            chunk.constants = constants;
        }

        {
            const list_size = try state.readInteger(reader);

            var prototypes = try allocator.alloc(*Chunk, list_size);

            var i: usize = 0;
            while (i < list_size) : (i += 1) {
                prototypes[i] = try Chunk.decode(allocator, state, reader);
            }

            chunk.prototypes = prototypes;
        }

        var debug_info: DebugInfo = undefined;

        {
            const list_size = try state.readInteger(reader);

            var lineinfo = try allocator.alloc(size_t, list_size);

            var i: usize = 0;
            while (i < list_size) : (i += 1) {
                lineinfo[i] = try state.readInteger(reader);
            }

            debug_info.lines = lineinfo;
        }

        {
            const list_size = try state.readInteger(reader);

            var locals = try allocator.alloc(DebugLocal, list_size);

            var i: usize = 0;
            while (i < list_size) : (i += 1) {
                locals[i] = .{
                    .name = try state.readString(allocator, reader),
                    .start_pc = try state.readInteger(reader),
                    .end_pc = try state.readInteger(reader),
                };
            }

            debug_info.locals = locals;
        }

        {
            const list_size = try state.readInteger(reader);

            var upvalues = try allocator.alloc(DebugUpvalue, list_size);

            var i: usize = 0;
            while (i < list_size) : (i += 1) {
                upvalues[i] = .{
                    .name = try state.readString(allocator, reader),
                };
            }

            debug_info.upvalues = upvalues;
        }

        chunk.debug = debug_info;

        return chunk;
    }

    pub fn deinit(self: *Chunk) void {
        self.allocator.free(self.name);

        for (self.constants) |constant| {
            switch (constant) {
                .string => |data| {
                    self.allocator.free(data);
                },
                else => {},
            }
        }

        for (self.prototypes) |proto| {
            proto.deinit();
        }

        for (self.debug.locals) |local| {
            self.allocator.free(local.name);
        }

        for (self.debug.upvalues) |upvalue| {
            self.allocator.free(upvalue.name);
        }

        self.allocator.free(self.instructions);
        self.allocator.free(self.constants);
        self.allocator.free(self.prototypes);

        self.allocator.free(self.debug.lines);
        self.allocator.free(self.debug.locals);
        self.allocator.free(self.debug.upvalues);

        self.allocator.destroy(self);
    }
};

pub const DebugLocal = struct {
    name: String,
    start_pc: size_t,
    end_pc: size_t,
};

pub const DebugUpvalue = struct {
    name: String,
};

pub const DebugInfo = struct {
    lines: []size_t,
    locals: []DebugLocal,
    upvalues: []DebugUpvalue,
};
