const std = @import("std");

const ch = @import("chunk.zig");
const v = @import("value.zig");
const print = std.debug.print;

pub const OpCode = ch.OpCode;

pub fn disassembleChunk(chunk: *ch.Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.len) : (offset = disassembleInstruction(chunk, offset)) {}
}

pub fn disassembleInstruction(chunk: *ch.Chunk, offset: usize) usize {
    print("{x:0>4} ", .{offset});

    if (offset > 0 and chunk.lines[offset] == chunk.lines[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:.4} ", .{chunk.lines[offset]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code[offset]);

    switch (instruction) {
        OpCode.CONSTANT => return constantInstruction("OP_CONSTANT", chunk, offset),
        OpCode.ADD => return simpleInstruction("OP_ADD", offset),
        OpCode.SUBTRACT => return simpleInstruction("OP_SUBTRACT", offset),
        OpCode.MULTIPLY => return simpleInstruction("OP_MULTIPLY", offset),
        OpCode.DIVIDE => return simpleInstruction("OP_DIVIDE", offset),
        OpCode.NEGATE => return simpleInstruction("OP_NEGATE", offset),
        OpCode.RETURN => return simpleInstruction("OP_RETURN", offset),
        else => {
            print("Unknown opcode {d}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *ch.Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    print("{s:<16} {d:>4} '", .{ name, constant });
    v.printValue(chunk.constants.values.items[constant]);
    print("'\n", .{});

    return offset + 2;
}
