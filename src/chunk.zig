const std = @import("std");

const memory = @import("memory.zig");
const value = @import("value.zig");
pub const OpCode = enum(u8) {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_LOCAL,
    SET_LOCAL,
    GET_GLOBAL,
    DEFINE_GLOBAL,
    SET_GLOBAL,
    GET_UPVALUE,
    SET_UPVALUE,
    EQUAL,
    GREATER,
    LESS,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NEGATE,
    PRINT,
    JUMP,
    JUMP_IF_FALSE,
    LOOP,
    CALL,
    CLOSURE,
    CLOSE_UPVALUE,
    NOT,
    RETURN,
    _,
};

pub const Chunk = struct {
    code: []u8,
    len: u8,
    capacity: usize = 0,
    constants: value.ValueArray,
    lines: []u32,

    pub fn init(allocator: *std.mem.Allocator) !Chunk {
        const chunk = Chunk{
            .code = try allocator.alloc(u8, 8),
            .len = 0,
            .capacity = 0,
            .constants = value.ValueArray.init(allocator.*),
            .lines = try allocator.alloc(u32, 8),
        };
        return chunk;
    }

    pub fn free(self: *Chunk, allocator: *std.mem.Allocator) void {
        if (self.capacity == 0) {
            return;
        }
        allocator.free(self.code);
        self.constants.free();
        allocator.free(self.lines);
        self.len = 0;
        self.capacity = 0;
    }

    pub fn write(self: *Chunk, allocator: *std.mem.Allocator, byte: u8, line: u32) void {
        if (self.capacity < self.len + 1) {
            const oldCapacity = self.capacity;
            self.capacity = memory.growCapacity(oldCapacity);
            self.code = memory.growArray(u8, allocator, self.code, self.capacity);
            self.lines = memory.growArray(u32, allocator, self.lines, self.capacity);
        }
        self.code[self.len] = byte;
        self.lines[self.len] = line;
        self.len += 1;
    }
    pub fn addConstant(self: *Chunk, constant: value.Value) u32 {
        self.constants.write(constant);
        return @intCast(self.constants.values.items.len - 1);
    }
};
