const ch = @import("chunk.zig");
const std = @import("std");
const print = std.debug.print;

pub const VM = struct {
    chunk: ch.Chunk,

    pub fn init(allocator: *std.mem.Allocator) !VM {
        const chunk = try ch.Chunk.init(allocator);
        const vm = VM{ .chunk = chunk };
        return vm;
    }

    pub fn free(self: *VM, allocator: *std.mem.Allocator) void {
        self.chunk.free(allocator);
    }
};
