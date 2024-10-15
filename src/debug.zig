const std = @import("std");

const chk = @import("chunk.zig");
const value = @import("value.zig");
const print = std.debug.print;

pub const OpCode = chk.OpCode;

pub fn disassembleChunk(chunk: *chk.Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.len) : (offset = disassembleInstruction(chunk, offset)) {}
}

pub fn disassembleInstruction(chunk: *chk.Chunk, offset: usize) usize {
    print("{x:0>4} ", .{offset});

    if (offset > 0 and chunk.lines[offset] == chunk.lines[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:.4} ", .{chunk.lines[offset]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code[offset]);

    switch (instruction) {
        .CONSTANT => return constantInstruction("OP_CONSTANT", chunk, offset),
        .ADD => return simpleInstruction("OP_ADD", offset),
        .SUBTRACT => return simpleInstruction("OP_SUBTRACT", offset),
        .MULTIPLY => return simpleInstruction("OP_MULTIPLY", offset),
        .DIVIDE => return simpleInstruction("OP_DIVIDE", offset),
        .NEGATE => return simpleInstruction("OP_NEGATE", offset),
        .RETURN => return simpleInstruction("OP_RETURN", offset),
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

fn constantInstruction(name: []const u8, chunk: *chk.Chunk, offset: usize) usize {
    const constant = chunk.code[offset + 1];
    print("{s:<16} {d:>4} '", .{ name, constant });
    value.printValue(chunk.constants.values.items[constant]);
    print("'\n", .{});

    return offset + 2;
}
