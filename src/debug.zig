const std = @import("std");

const chk = @import("chunk.zig");
const value = @import("value.zig");
const print = std.debug.print;

pub const OpCode = chk.OpCode;

pub fn disassembleChunk(chunk: *chk.Chunk, name: []const u8) !void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.len) : (offset = try disassembleInstruction(chunk, offset)) {}
}

pub fn disassembleInstruction(chunk: *chk.Chunk, offset: usize) !usize {
    print("{x:0>4} ", .{offset});

    if (offset > 0 and chunk.lines[offset] == chunk.lines[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:.4} ", .{chunk.lines[offset]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code[offset]);

    switch (instruction) {
        .CONSTANT => return try constantInstruction(@tagName(.CONSTANT), chunk, offset),
        else => {
            return simpleInstruction(@tagName(instruction), offset);
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *chk.Chunk, offset: usize) !usize {
    const constant = chunk.code[offset + 1];
    print("{s:<16} {d:>4} '", .{ name, constant });
    try value.printValue(chunk.constants.values.items[constant], std.io.getStdErr().writer());
    print("'\n", .{});

    return offset + 2;
}