const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const cmp = @import("compiler.zig");
const debug = @import("debug.zig");
const DEBUG_TRACE_EXECUTION = @import("main.zig").DEBUG_TRACE_EXECUTION;
const obj = @import("object.zig");
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const printValue = @import("value.zig").printValue;
const toString = @import("value.zig").toString;
const valuesEqual = @import("value.zig").valuesEqual;
const print = std.debug.print;

pub const InterpretResult = enum(u8) { INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR };

const STACK_MAX = 256;

pub const VM = struct {
    allocator: *std.mem.Allocator,
    chunk: ?*Chunk,
    ip: usize,
    stack: [STACK_MAX]Value,
    stackTop: u8,
    writer: std.fs.File.Writer,
    objects: ?*obj.Obj,
    strings: *Table,
    globals: *Table,

    pub fn init(allocator: *std.mem.Allocator, writer: std.fs.File.Writer) !*VM {
        const stack = [_]Value{.{ .none = {} }} ** STACK_MAX;
        const v = try allocator.create(VM);
        v.* = VM{
            .allocator = allocator,
            .chunk = null,
            .ip = 0,
            .stack = stack,
            .stackTop = 0,
            .writer = writer,
            .objects = null,
            .strings = try Table.init(allocator),
            .globals = try Table.init(allocator),
        };
        return v;
    }

    pub fn free(self: *VM) void {
        self.freeObjects();
        self.strings.free();
        self.globals.free();
        self.allocator.destroy(self);
    }

    fn freeObjects(self: *VM) void {
        var current = self.objects;
        while (current) |object| {
            const nextObject = object.next;
            obj.freeObject(object);
            current = nextObject;
        }
    }

    pub fn interpret(self: *VM, source: [:0]u8) !InterpretResult {
        var chunk = try Chunk.init(self.allocator);
        defer chunk.free(self.allocator);
        if (!cmp.global_compiler.compile(source, &chunk)) {
            return InterpretResult.INTERPRET_COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = 0;

        return try self.run();
    }

    pub fn push(self: *VM, value: Value) void {
        self.stack[self.stackTop] = value;
        self.stackTop += 1;
    }

    pub fn pop(self: *VM) Value {
        self.stackTop -= 1;
        return self.stack[self.stackTop];
    }

    pub fn resetStack(self: *VM) void {
        self.stackTop = 0;
    }

    pub fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) void {
        std.debug.print(format, args);

        const instruction = self.ip - 1;
        const line = self.chunk.?.lines[instruction];
        std.debug.print("\n[line {d}] in script\n", .{line});
        self.resetStack();
    }

    pub fn run(self: *VM) !InterpretResult {
        while (true) {
            if (DEBUG_TRACE_EXECUTION) {
                print("        ", .{});
                for (self.stack.items) |slot| {
                    const msg = try toString(slot, self.allocator);
                    print("[ {s} ]", .{msg});
                    if (slot.isNumber()) self.allocator.free(msg);
                }
                print("\n", .{});

                const code_slice = self.chunk orelse unreachable;
                const offset = self.ip;

                _ = try debug.disassembleInstruction(code_slice, offset);
            }
            const instruction: OpCode = @enumFromInt(self.readByte());

            switch (instruction) {
                .EQUAL => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(Value.bool(valuesEqual(a, b)));
                },
                .GREATER => _ = try self.binaryOp(bool, opGreater),
                .LESS => _ = try self.binaryOp(bool, opLess),
                .ADD => {
                    if (self.peek(0).isString() and self.peek(1).isString()) {
                        try self.concatenate();
                    } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                        const b = self.pop().asNumber();
                        const a = self.pop().asNumber();
                        self.push(Value.number(a + b));
                    } else {
                        self.runtimeError("Operands must be two numbers or two strings", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .SUBTRACT => _ = try self.binaryOp(f64, opSubtract),
                .MULTIPLY => _ = try self.binaryOp(f64, opMultiply),
                .DIVIDE => _ = try self.binaryOp(f64, opDivide),
                .NOT => self.push(Value.bool(isFalsey(self.pop()))),
                .NEGATE => {
                    if (!self.peek(0).isNumber()) {
                        self.runtimeError("Operand must be a number.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    const negated_value = -self.pop().asNumber();
                    self.push(Value.number(negated_value));
                },
                .PRINT => {
                    try printValue(self.pop(), self.writer);
                    try self.writer.writeAll("\n");
                },
                .RETURN => {
                    return .INTERPRET_OK;
                },
                .CONSTANT => {
                    const constant = self.readConstant();
                    self.push(constant);
                },
                .NIL => self.push(Value.nil()),
                .TRUE => self.push(Value.bool(true)),
                .FALSE => self.push(Value.bool(false)),
                .POP => _ = self.pop(),
                .DEFINE_GLOBAL => {
                    const name = self.readString();

                    _ = try self.globals.set(name, self.peek(0));
                    _ = self.pop();
                },
                .GET_GLOBAL => {
                    const name = self.readString();
                    if (self.globals.get(name)) |value| {
                        self.push(value.*);
                    } else {
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .SET_GLOBAL => {
                    const name = self.readString();
                    const keyFound = try self.globals.set(name, self.peek(0));
                    if (keyFound) {
                        print("uh oh!\n", .{});
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .GET_LOCAL => {
                    const slot = self.readByte();
                    self.push(self.stack[slot]);
                },
                .SET_LOCAL => {
                    const slot = self.readByte();
                    self.stack[slot] = self.peek(0);
                },
                .JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (isFalsey(self.peek(0))) self.ip += offset;
                },
                .JUMP => {
                    const offset = self.readShort();
                    self.ip += offset;
                },
                .LOOP => {
                    const offset = self.readShort();
                    self.ip -= offset;
                },
                else => {
                    print("unknown opcode: {d}", .{instruction});
                    return .INTERPRET_COMPILE_ERROR;
                },
            }
        }
    }

    fn peek(self: *VM, distance: usize) Value {
        return self.stack[self.stackTop - 1 - distance];
    }

    fn isFalsey(value: Value) bool {
        return value.isNil() or (value.isBool() and !value.asBool());
    }

    fn concatenate(self: *VM) !void {
        const b = self.pop().asString();
        const a = self.pop().asString();

        const length = a.chars.len + b.chars.len;
        var chars = try self.allocator.alloc(u8, length);
        @memcpy(chars[0..a.chars.len], a.chars);
        @memcpy(chars[a.chars.len..length], b.chars);

        const result = try obj.takeString(chars);
        self.push(Value.object(&result.obj));
    }

    fn readByte(self: *VM) u8 {
        const byte = self.chunk.?.code[self.ip];
        self.ip += 1;
        return byte;
    }

    fn readConstant(self: *VM) Value {
        const chunk = self.chunk orelse unreachable;
        return chunk.constants.values.items[self.readByte()];
    }

    fn readString(self: *VM) *obj.ObjString {
        return self.readConstant().asString();
    }

    fn readShort(self: *VM) u16 {
        const high = self.chunk.?.code[self.ip];
        const low = self.chunk.?.code[self.ip + 1];
        self.ip += 2;
        return (@as(u16, high) << 8) | @as(u16, low);
    }

    fn binaryOp(
        self: *VM,
        comptime ReturnType: type,
        comptime op: fn (a: f64, b: f64) ReturnType,
    ) !InterpretResult {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbres", .{});
            return .INTERPRET_RUNTIME_ERROR;
        }
        const b = self.pop().asNumber();
        const a = self.pop().asNumber();
        const result = op(a, b);
        const value = switch (ReturnType) {
            bool => Value.bool(result),
            f64 => Value.number(result),
            else => @compileError("Unsupported result type in binaryOp"),
        };
        self.push(value);
        return .INTERPRET_OK;
    }
};

fn opAdd(a: f64, b: f64) f64 {
    return a + b;
}

fn opSubtract(a: f64, b: f64) f64 {
    return a - b;
}

fn opMultiply(a: f64, b: f64) f64 {
    return a * b;
}

fn opDivide(a: f64, b: f64) f64 {
    return a / b;
}

fn opGreater(a: f64, b: f64) bool {
    return a > b;
}

fn opLess(a: f64, b: f64) bool {
    return a < b;
}
