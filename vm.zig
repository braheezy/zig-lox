const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const compile = @import("compile.zig").compile;
const debug = @import("debug.zig");
const DEBUG_TRACE_EXECUTION = @import("main.zig").DEBUG_TRACE_EXECUTION;
const Value = @import("value.zig").Value;
const printValue = @import("value.zig").printValue;
const print = std.debug.print;
const OpCode = debug.OpCode;

pub const InterpretResult = enum(u8) { INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR };

pub const VM = struct {
    chunk: ?*Chunk,
    ip: ?[]u8,
    stack: std.ArrayList(Value),

    pub fn init(allocator: *std.mem.Allocator) VM {
        const stack = std.ArrayList(Value).init(allocator.*);
        const vm = VM{ .chunk = null, .ip = null, .stack = stack };
        return vm;
    }

    pub fn free(self: *VM, allocator: *std.mem.Allocator) void {
        if (self.chunk) |chunk| {
            chunk.free(allocator);
        }
        self.stack.deinit();
    }

    pub fn interpret(self: *VM, source: [:0]u8) InterpretResult {
        _ = self;
        compile(source);
        return InterpretResult.INTERPRET_OK;
    }

    pub fn resetStack(self: *VM) void {
        self.stack.clearRetainingCapacity();
    }

    pub fn run(self: *VM) !InterpretResult {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                print("        ", .{});
                for (self.stack.items) |slot| {
                    print("[ {d} ]", .{slot});
                }
                print("\n", .{});

                const code_slice = self.chunk orelse unreachable;
                const ip_slice = self.ip orelse unreachable;
                const offset = code_slice.code.len - ip_slice.len;

                _ = debug.disassembleInstruction(code_slice, offset);
            }
            const instruction: OpCode = @enumFromInt(self.readByte());

            switch (instruction) {
                OpCode.OP_ADD => try self.binaryOp(opAdd),
                OpCode.OP_SUBTRACT => try self.binaryOp(opSubtract),
                OpCode.OP_MULTIPLY => try self.binaryOp(opMultiply),
                OpCode.OP_DIVIDE => try self.binaryOp(opDivide),
                OpCode.OP_NEGATE => self.stack.items[self.stack.items.len - 1] = -self.stack.getLast(),
                OpCode.OP_RETURN => {
                    printValue(self.stack.pop());
                    print("\n", .{});
                    return InterpretResult.INTERPRET_OK;
                },
                OpCode.OP_CONSTANT => {
                    const constant = self.readConstant();
                    try self.stack.append(constant);
                },
                else => print("unknown opcode", .{}),
            }
        }
    }

    fn readByte(self: *VM) u8 {
        const ip_slice = self.ip orelse unreachable;
        const byte = ip_slice[0];
        self.ip = ip_slice[1..];
        return byte;
    }

    fn readConstant(self: *VM) f64 {
        const chunk = self.chunk orelse unreachable;
        return chunk.constants.values.items[self.readByte()];
    }

    fn binaryOp(self: *VM, comptime op: fn (a: Value, b: Value) Value) !void {
        const b = self.stack.pop();
        const a = self.stack.pop();
        const result = op(a, b);
        try self.stack.append(result);
    }
};

fn opAdd(a: Value, b: Value) Value {
    return a + b;
}

fn opSubtract(a: Value, b: Value) Value {
    return a - b;
}

fn opMultiply(a: Value, b: Value) Value {
    return a * b;
}

fn opDivide(a: Value, b: Value) Value {
    return a / b;
}
