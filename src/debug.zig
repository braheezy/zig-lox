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
        print("{d:>4} ", .{chunk.lines[offset]});
    }

    const instruction: OpCode = @enumFromInt(chunk.code[offset]);

    switch (instruction) {
        .CONSTANT => return try constantInstruction(@tagName(.CONSTANT), chunk, offset),
        .DEFINE_GLOBAL => return try constantInstruction(@tagName(.DEFINE_GLOBAL), chunk, offset),
        .GET_GLOBAL => return try constantInstruction(@tagName(.GET_GLOBAL), chunk, offset),
        .SET_GLOBAL => return try constantInstruction(@tagName(.SET_GLOBAL), chunk, offset),
        .SET_LOCAL => return byteInstruction(@tagName(.SET_LOCAL), chunk, offset),
        .GET_LOCAL => return byteInstruction(@tagName(.GET_LOCAL), chunk, offset),
        .GET_UPVALUE => return byteInstruction(@tagName(.GET_UPVALUE), chunk, offset),
        .SET_UPVALUE => return byteInstruction(@tagName(.SET_UPVALUE), chunk, offset),
        .CALL => return byteInstruction(@tagName(.CALL), chunk, offset),
        .JUMP => return jumpInstruction(@tagName(.JUMP), 1, chunk, offset),
        .JUMP_IF_FALSE => return jumpInstruction(@tagName(.JUMP_IF_FALSE), 1, chunk, offset),
        .LOOP => return jumpInstruction(@tagName(.JUMP_IF_FALSE), -1, chunk, offset),
        .CLOSURE => {
            var newOffset = offset + 1;

            const constant = chunk.code[newOffset];
            newOffset += 1;
            print("{s:<20} {d:>4} ", .{ "CLOSURE", constant });
            try value.printValue(chunk.constants.values.items[constant], std.io.getStdErr().writer());
            print("\n", .{});

            const function = chunk.constants.values.items[constant].asFunction();
            for (0..function.upvalueCount) |_| {
                const isLocal = chunk.code[newOffset];
                newOffset += 1;
                const index = chunk.code[newOffset];
                newOffset += 1;
                print("{d:>4}      |                     {s} {d}\n", .{
                    newOffset - 2,
                    if (isLocal > 0) "local" else "upvalue",
                    index,
                });
            }

            return newOffset;
        },
        else => {
            return simpleInstruction(@tagName(instruction), offset);
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s:<20}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *chk.Chunk, offset: usize) !usize {
    const constant = chunk.code[offset + 1];
    print("{s:<20} {d:>4} '", .{ name, constant });
    try value.printValue(chunk.constants.values.items[constant], std.io.getStdErr().writer());
    print("'\n", .{});

    return offset + 2;
}

fn byteInstruction(name: []const u8, chunk: *chk.Chunk, offset: usize) usize {
    const slot = chunk.code[offset + 1];
    print("{s<20} {d:>4}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: isize, chunk: *chk.Chunk, offset: usize) usize {
    var jump: usize = @as(u16, chunk.code[offset + 1]) << 8;
    jump |= chunk.code[offset + 2];
    print("{s<20} {d:>4} -> {d}\n", .{ name, offset, @as(i128, offset) + 3 + sign * @as(i128, jump) });
    return offset + 3;
}
