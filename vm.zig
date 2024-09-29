const ch = @import("chunk.zig");
const std = @import("std");
const print = std.debug.print;

pub const VM = struct {
    chunk: *ch.Chunk,

    pub fn init(allocator: *std.mem.Allocator) VM {
        return VM{
            .chunk = ch.Chunk.init(allocator),
        };
    }

    pub fn free(self: *VM) void {
        // if (self.chunk != null) {
        // self.chunk.free(allocator);
        // allocator.free(self.chunk); // Free the chunk pointer itself
        // }
        print("freeing vm: {}", .{self});
    }
};
