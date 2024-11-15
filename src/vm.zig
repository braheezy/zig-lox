const std = @import("std");

const OpCode = @import("chunk.zig").OpCode;
const cmp = @import("compiler.zig");
const debug = @import("debug.zig");
const main = @import("main.zig");
const memory = @import("memory.zig");
const obj = @import("object.zig");
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const printValue = @import("value.zig").printValue;
const toString = @import("value.zig").toString;
const valuesEqual = @import("value.zig").valuesEqual;
const print = std.debug.print;

pub const InterpretResult = enum(u8) { INTERPRET_OK, INTERPRET_COMPILE_ERROR, INTERPRET_RUNTIME_ERROR };

const STACK_MAX = FRAMES_MAX * cmp.UINT8_COUNT;
const FRAMES_MAX = 64;

pub const CallFrame = struct {
    closure: *obj.ObjClosure,
    ip: usize,
    slots: []Value,
    slot_index: usize,
};

pub const VM = struct {
    allocator: *memory.VMAllocator,
    frames: [FRAMES_MAX]CallFrame,
    frame_count: u8,
    stack: [STACK_MAX]Value,
    stack_top: usize,
    writer: std.fs.File.Writer,
    objects: ?*obj.Obj,
    init_string: ?*obj.ObjString,
    open_upvalues: ?*obj.ObjUpvalue,
    strings: *Table,
    globals: *Table,
    gray_count: usize,
    gray_capacity: usize,
    gray_stack: []?*obj.Obj,

    pub fn init(allocator: *std.mem.Allocator, writer: std.fs.File.Writer) !*VM {
        const stack = [_]Value{.{ .none = {} }} ** STACK_MAX;
        const frames = [_]CallFrame{.{
            .closure = undefined,
            .ip = 0,
            .slots = undefined,
            .slot_index = 0,
        }} ** FRAMES_MAX;
        var vm_allocator = memory.VMAllocator.init(allocator);

        var v = VM{
            .allocator = &vm_allocator,
            .stack = stack,
            .stack_top = 0,
            .writer = writer,
            .objects = null,
            .strings = try Table.init(&vm_allocator),
            .globals = try Table.init(&vm_allocator),
            .frames = frames,
            .frame_count = 0,
            .init_string = null,
            .open_upvalues = null,
            .gray_capacity = 0,
            .gray_count = 0,
            .gray_stack = &[_]?*obj.Obj{},
        };

        vm_allocator.vm = &v;

        try v.defineNative("clock", clockNative);
        v.init_string = try obj.copyString(&v, "init");
        return &v;
    }

    pub fn free(self: *VM) void {
        self.strings.free();
        self.globals.free();
        self.init_string = null;
        self.freeObjects();
    }

    pub fn resetStack(self: *VM) void {
        self.stack_top = 0;
        self.open_upvalues = null;
    }

    fn freeObjects(self: *VM) void {
        var current = self.objects;
        while (current) |object| {
            const next_object = object.next;
            obj.freeObject(self, object);
            current = next_object;
        }
        self.allocator.allocator.free(self.gray_stack);
    }

    pub fn interpret(self: *VM, source: [:0]u8) !InterpretResult {
        try cmp.Compiler.init(self, .Script);
        const function = try cmp.current_compiler.?.compile(source);
        if (function) |_| {
            self.push(Value.object(&function.?.obj));
            // print("[interpret] function.upvalue_count: {d}\n", .{function.?.upvalue_count});
            const closure = try obj.newClosure(self, function.?);
            _ = self.pop();
            self.push(Value.object(&closure.obj));
            _ = self.call(closure, 0);
        } else {
            return InterpretResult.INTERPRET_COMPILE_ERROR;
        }

        return try self.run();
    }

    pub fn push(self: *VM, value: Value) void {
        // print("[stack.push]\n", .{});
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    pub fn pop(self: *VM) Value {
        if (self.stack_top > 0) self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    pub fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) void {
        std.debug.print(format, args);

        std.debug.print("\n", .{});

        var i = self.frame_count - 1;
        while (i >= 0) {
            const frame = &self.frames[i];
            const func = frame.closure.function;
            const instruction = frame.ip - 1;
            print("<line {d}> in ", .{func.chunk.lines[instruction]});
            if (func.name) |name| {
                print("{s}()\n", .{name.chars});
            } else {
                print("script\n", .{});
            }
            if (i == 0) break;
            i -= 1;
        }
        self.resetStack();
    }

    pub fn defineNative(self: *VM, name: []const u8, function: obj.NativeFn) !void {
        // print("[defineNative 1] self.stack[0]: {any}\n", .{self.stack[0]});
        const result = try obj.copyString(self, name);
        // print("[defineNative 2] self.stack[0]: {any}\n", .{self.stack[0]});
        self.push(Value.object(&result.obj));
        const native_func = try obj.newNative(self, function);
        // print("[defineNative 3] self.stack[0]: {any}\n", .{self.stack[0]});
        // print("[defineNative 4]\n", .{});
        self.push(Value.object(&native_func.obj));
        _ = self.globals.set(self.stack[0].asString(), self.stack[1]);
        _ = self.pop();
        _ = self.pop();
        // print("[defineNative END]\n", .{});
    }

    pub fn run(self: *VM) !InterpretResult {
        while (true) {
            if (main.DEBUG_TRACE_EXECUTION) {
                print("    ST: ", .{});
                for (self.stack, 0..) |slot, i| {
                    const msg = try toString(slot, self.allocator.allocator);
                    print("[ {s} ]", .{msg});
                    if (slot.isNumber()) self.allocator.allocator.free(msg);

                    if (i == self.stack_top - 1) break;
                }
                print("\n", .{});

                const frame = getFrame(self);
                const chunk = &frame.closure.function.chunk;

                _ = try debug.disassembleInstruction(chunk, frame.ip);
            }
            const instruction: OpCode = @enumFromInt(self.readByte());
            const frame = self.getFrame();

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
                .CLASS => {
                    const name = self.readString();
                    const class = try obj.newClass(self, name);
                    self.push(Value.object(&class.obj));
                },
                .CALL => {
                    const arg_count = self.readByte();
                    const result = try self.callValue(self.peek(arg_count), arg_count);
                    if (!result) {
                        return .INTERPRET_COMPILE_ERROR;
                    }
                },
                .GET_PROPERTY => {
                    if (!self.peek(0).isInstance()) {
                        self.runtimeError("Only instances have properties.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    const instance = self.peek(0).asInstance();
                    const name = self.readString();
                    if (instance.fields.get(name)) |value| {
                        _ = self.pop();
                        self.push(value.*);
                    } else {
                        const successful_bind = try self.bindMethod(instance.class, name);
                        if (!successful_bind) {
                            return .INTERPRET_RUNTIME_ERROR;
                        }
                    }
                },
                .INHERIT => {
                    const superclass = self.peek(1);
                    if (!superclass.isClass()) {
                        self.runtimeError("Superclass must be a class.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    const subclass = self.peek(0).asClass();

                    Table.setAll(superclass.asClass().methods, subclass.methods);

                    _ = self.pop();
                },
                .METHOD => {
                    self.defineMethod(self.readString());
                },
                .SET_PROPERTY => {
                    if (!self.peek(1).isInstance()) {
                        self.runtimeError("Only instances have fields.", .{});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                    const instance = self.peek(1).asInstance();
                    _ = instance.fields.set(self.readString(), self.peek(0));
                    const value = self.pop();
                    _ = self.pop();
                    self.push(value);
                },
                .INVOKE => {
                    const method = self.readString();
                    const arg_count = self.readByte();
                    const invoke_success = try self.invoke(method, arg_count);
                    if (!invoke_success) {
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .GET_SUPER => {
                    const name = self.readString();
                    const superclass = self.pop().asClass();
                    const successful_bind = try self.bindMethod(superclass, name);
                    if (!successful_bind) {
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .SUPER_INVOKE => {
                    const method = self.readString();
                    const arg_count = self.readByte();
                    const superclass = self.pop().asClass();
                    if (self.invokeFromClass(superclass, method, arg_count)) {
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .CLOSURE => {
                    const func = self.readConstant().asFunction();
                    const closure = try obj.newClosure(self, func);
                    self.push(Value.object(&closure.obj));
                    for (0..closure.upvalue_count) |i| {
                        const is_local = self.readByte();
                        const index = self.readByte();
                        if (is_local != 0) {
                            closure.upvalues[i] = try self.captureUpvalue(&frame.slots[index]);
                        } else {
                            // print("len of frame.closure.upvalues: {d}. func.upvalue_count: {d}\n", .{ frame.closure.upvalues.len, func.upvalue_count });
                            closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }
                },
                .GET_UPVALUE => {
                    // print("[run] GET_UPVALUE\n", .{});
                    const slot = self.readByte();
                    self.push(frame.closure.upvalues[slot].?.location.*);
                },
                .SET_UPVALUE => {
                    const slot = self.readByte();
                    frame.closure.upvalues[slot].?.location.* = self.peek(0);
                },
                .CLOSE_UPVALUE => {
                    self.closeUpvalues(&self.stack[self.stack_top - 1]);
                    _ = self.pop();
                },
                .RETURN => {
                    const result = self.pop();
                    //? Or should the index be with frame.ip?
                    self.closeUpvalues(&frame.slots[frame.slot_index]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return .INTERPRET_OK;
                    }
                    // const frame = self.getFrame();
                    self.stack_top = frame.slot_index;
                    // self.stack_top = (@intFromPtr(frame.slots.ptr) - @intFromPtr(&self.stack[0])) / @sizeOf(Value);
                    self.push(result);
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

                    _ = self.globals.set(name, self.peek(0));
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
                    const key_found = self.globals.set(name, self.peek(0));
                    if (key_found) {
                        _ = self.globals.delete(name);
                        self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                        return .INTERPRET_RUNTIME_ERROR;
                    }
                },
                .GET_LOCAL => {
                    const slot = self.readByte();
                    self.push(frame.slots[slot]);
                },
                .SET_LOCAL => {
                    const slot = self.readByte();
                    frame.slots[slot] = self.peek(0);
                },
                .JUMP_IF_FALSE => {
                    const offset = self.readShort();
                    if (isFalsey(self.peek(0))) frame.ip += offset;
                },
                .JUMP => {
                    const offset = self.readShort();
                    frame.ip += offset;
                },
                .LOOP => {
                    const offset = self.readShort();
                    frame.ip -= offset;
                },
                else => {
                    print("unknown opcode: {d}", .{instruction});
                    return .INTERPRET_COMPILE_ERROR;
                },
            }
        }
    }

    fn getFrame(self: *VM) *CallFrame {
        // because frame_count is usize, frame_count - 1 could be interger overflow
        return if (self.frame_count > 0) &self.frames[self.frame_count - 1] else unreachable;
    }

    fn peek(self: *VM, distance: usize) Value {
        if (self.stack_top > 0) {
            return self.stack[self.stack_top - 1 - distance];
        } else {
            return self.stack[0];
        }
    }

    fn call(self: *VM, closure: *obj.ObjClosure, arg_count: u8) bool {
        if (arg_count != closure.function.arity) {
            self.runtimeError("Expected {d} arguments but got {d}.\n", .{ closure.function.arity, arg_count });
            return false;
        }
        if (self.frame_count == FRAMES_MAX) {
            self.runtimeError("Stack overflow ;)", .{});
            return false;
        }
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        // print("[call] closure.upvalue_count: {d}\n", .{closure.upvalue_count});
        frame.closure = closure;
        frame.ip = 0;

        frame.slot_index = self.stack_top - arg_count - 1;

        frame.slots = self.stack[frame.slot_index..];
        return true;
    }

    fn defineMethod(self: *VM, name: *obj.ObjString) void {
        // print("[defineMethod] {s}\n", .{name.chars});
        const method = self.peek(0);
        const class = self.peek(1).asClass();
        _ = class.methods.set(name, method);
        _ = self.pop();
    }

    fn bindMethod(self: *VM, class: *obj.ObjClass, name: *obj.ObjString) !bool {
        // print("[bindMethod] {s}\n", .{name.chars});
        if (class.methods.get(name)) |method| {
            const bound = try obj.newBoundMethod(
                self,
                self.peek(0),
                method.asClosure(),
            );
            _ = self.pop();
            self.push(Value.object(&bound.obj));
            return true;
        } else {
            self.runtimeError("1 Undefined property '{s}'.\n", .{name.chars});
            return false;
        }
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !bool {
        if (callee.isObject()) {
            switch (callee.objType()) {
                .bound_method => {
                    const bound = callee.asBoundMethod();
                    self.stack[self.stack_top - arg_count - 1] = bound.receiver;
                    return self.call(bound.method, arg_count);
                },
                .class => {
                    const class = callee.asClass();
                    const inst = try obj.newInstance(self, class);
                    self.stack[self.stack_top - arg_count - 1] = Value.object(&inst.obj);
                    if (class.methods.get(self.init_string.?)) |initializer| {
                        return self.call(initializer.asClosure(), arg_count);
                    } else if (arg_count != 0) {
                        self.runtimeError("Expected 0 arguments but got {d}.\n", .{arg_count});
                        return false;
                    }
                    return true;
                },
                .closure => return self.call(callee.asClosure(), arg_count),
                .native => {
                    const native = callee.asNative();
                    const args = self.stack[self.stack_top - arg_count .. self.stack_top];
                    const result = native(arg_count, args);
                    self.stack_top -= arg_count + 1;
                    self.push(result);
                    return true;
                },
                else => {},
            }
        }
        self.runtimeError("Can only call functions and classes\n", .{});
        return false;
    }

    fn invoke(self: *VM, name: *obj.ObjString, arg_count: u8) !bool {
        const receiver = self.peek(arg_count);
        if (!receiver.isInstance()) {
            self.runtimeError("Only instances have methods.\n", .{});
            return false;
        }
        const instance = receiver.asInstance();
        if (instance.fields.get(name)) |value| {
            // ? Correct math
            self.stack[self.stack_top - arg_count - 1] = value.*;
            return try self.callValue(value.*, arg_count);
        }
        return self.invokeFromClass(instance.class, name, arg_count);
    }

    fn invokeFromClass(self: *VM, class: *obj.ObjClass, name: *obj.ObjString, arg_count: u8) bool {
        if (class.methods.get(name)) |method| {
            return self.call(method.asClosure(), arg_count);
        } else {
            self.runtimeError("Undefined property '{s}'.\n", .{name.chars});
            return false;
        }
    }

    fn captureUpvalue(self: *VM, local: *Value) !*obj.ObjUpvalue {
        var prev_upvalue: ?*obj.ObjUpvalue = null;
        var upvalue = self.open_upvalues;
        while (upvalue) |current_upvalue| {
            if (@intFromPtr(current_upvalue.location) <= @intFromPtr(local)) break;
            prev_upvalue = current_upvalue;
            upvalue = current_upvalue.next;
        }
        if (upvalue) |current_upvalue| {
            if (current_upvalue.location == local) return current_upvalue;
        }

        var created_upvalue = try obj.newUpvalue(self, local);
        created_upvalue.next = upvalue;
        if (prev_upvalue) |*pre| pre.*.next = created_upvalue else self.open_upvalues = created_upvalue;
        return created_upvalue;
    }

    fn closeUpvalues(self: *VM, last: *Value) void {
        while (self.open_upvalues) |current_upvalue| {
            if (@intFromPtr(current_upvalue.location) < @intFromPtr(last)) break;
            var upvalue = current_upvalue;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed;
            self.open_upvalues = upvalue.next;
        }
    }

    fn isFalsey(value: Value) bool {
        return value.isNil() or (value.isBool() and !value.asBool());
    }

    fn concatenate(self: *VM) !void {
        const b = self.peek(0).asString();
        const a = self.peek(1).asString();

        const length = a.chars.len + b.chars.len;
        var chars = self.allocator.reallocate(u8, null, length);
        @memcpy(chars[0..a.chars.len], a.chars);
        @memcpy(chars[a.chars.len..length], b.chars);

        const result = try obj.takeString(self, chars);
        _ = self.pop();
        _ = self.pop();
        self.push(Value.object(&result.obj));
    }

    fn readByte(self: *VM) u8 {
        const frame = self.getFrame();
        // std.debug.print("frame_count: {}, frame.ip: {}, code.len: {}\n", .{ self.frame_count, frame.ip, frame.function.chunk.code.len });

        const byte = frame.closure.function.chunk.code[frame.ip];
        frame.ip += 1;
        return byte;
    }

    fn readConstant(self: *VM) Value {
        const constant_index = readByte(self);
        const frame = self.getFrame();
        // print("[readConstant] frame.closure.function.chunk.constants.values: {any}\n", .{frame.closure.function.chunk.constants.values});
        return frame.closure.function.chunk.constants.values[constant_index];
    }

    fn readShort(self: *VM) u16 {
        const frame = self.getFrame();
        const high = frame.closure.function.chunk.code[frame.ip];
        const low = frame.closure.function.chunk.code[frame.ip + 1];
        frame.ip += 2;
        return (@as(u16, high) << 8) | @as(u16, low);
    }

    fn readString(self: *VM) *obj.ObjString {
        return self.readConstant().asString();
    }

    fn binaryOp(
        self: *VM,
        comptime ReturnType: type,
        comptime op: fn (a: f64, b: f64) ReturnType,
    ) !InterpretResult {
        if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
            self.runtimeError("Operands must be numbers. op 1: {any}, op 2: {any}\n", .{ self.peek(0), self.peek(1) });
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

pub fn clockNative(arg_count: u8, args: []Value) Value {
    _ = arg_count;
    _ = args;

    const now: i64 = std.time.timestamp();
    const seconds: u64 = @intCast(now);
    return Value.number(@floatFromInt(seconds));
}
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
