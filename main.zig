const std = @import("std");
const ch = @import("chunk.zig");
const OpCode = ch.OpCode;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;

pub const DEBUG_TRACE_EXECUTION = true;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var vm = VM.init(&allocator);
    defer vm.free(&allocator);

    var chunk = try ch.Chunk.init(&allocator);
    defer chunk.free(&allocator);

    var constant = chunk.addConstant(1.2);
    chunk.write(&allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    chunk.write(&allocator, constant, 123);

    constant = chunk.addConstant(3.4);
    chunk.write(&allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    chunk.write(&allocator, constant, 123);

    chunk.write(&allocator, @intFromEnum(OpCode.OP_ADD), 123);

    constant = chunk.addConstant(5.6);
    chunk.write(&allocator, @intFromEnum(OpCode.OP_CONSTANT), 123);
    chunk.write(&allocator, constant, 123);

    chunk.write(&allocator, @intFromEnum(OpCode.OP_DIVIDE), 123);

    chunk.write(&allocator, @intFromEnum(OpCode.OP_NEGATE), 123);

    chunk.write(&allocator, @intFromEnum(OpCode.OP_RETURN), 123);

    debug.disassembleChunk(&chunk, "test chunk");
    _ = try vm.interpret(&chunk);
}
