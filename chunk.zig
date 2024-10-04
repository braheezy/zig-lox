const std = @import("std");
const v = @import("value.zig");

pub const OpCode = enum(u8) { OP_CONSTANT, OP_ADD, OP_SUBTRACT, OP_MULTIPLY, OP_DIVIDE, OP_NEGATE, OP_RETURN, _ };

pub const Chunk = struct {
    code: []u8,
    len: usize,
    capacity: u8 = 0,
    constants: v.ValueArray,
    lines: []i32,

    pub fn init(allocator: *std.mem.Allocator) !Chunk {
        const chunk = Chunk{
            .code = try allocator.alloc(u8, 8),
            .len = 0,
            .capacity = 0,
            .constants = v.ValueArray.init(allocator),
            .lines = try allocator.alloc(i32, 8),
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

    pub fn write(self: *Chunk, allocator: *std.mem.Allocator, byte: u8, line: i32) void {
        if (self.capacity < self.len + 1) {
            const oldCapacity = self.capacity;
            self.capacity = growCapacity(oldCapacity);
            self.code = growArray(u8, allocator, self.code, self.capacity);
            self.lines = growArray(i32, allocator, self.lines, self.capacity);
        }
        self.code[self.len] = byte;
        self.lines[self.len] = line;
        self.len += 1;
    }
    pub fn addConstant(self: *Chunk, value: v.Value) u8 {
        self.constants.write(value);
        return @intCast(self.constants.values.items.len - 1);
    }

    fn growCapacity(capacity: u8) u8 {
        return if (capacity < 8) 8 else capacity * 2;
    }
    fn growArray(
        comptime T: type,
        allocator: *std.mem.Allocator,
        oldArray: []T,
        newCount: usize,
    ) []T {
        const newArray = allocator.realloc(oldArray, newCount) catch {
            std.debug.print("Failed to allocate memory", .{});
            std.process.exit(1);
        };
        return newArray;
    }
};
