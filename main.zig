const std = @import("std");
const ch = @import("chunk.zig");
const OpCode = ch.OpCode;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var vm = try VM.init(&allocator);
    defer vm.free(&allocator);

    var chunk = try ch.Chunk.init(&allocator);
    defer chunk.free(&allocator);

    const constant = chunk.addConstant(1.2);
    chunk.write(&allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    chunk.write(&allocator, constant, 123);

    chunk.write(&allocator, @intFromEnum(OpCode.OP_RETURN), 123);

    debug.disassembleChunk(&chunk, "test chunk");
}
