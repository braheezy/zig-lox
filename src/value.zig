const std = @import("std");

const memory = @import("memory.zig");
const obj = @import("object.zig");
const NAN_BOXING = @import("main.zig").NAN_BOXING;

const QNAN = 0b111111111111100000000000000000000000000000000000000000000000000;
const SIGN_BIT = 0b1000000000000000000000000000000000000000000000000000000000000000;
const TAG_NIL = 0b1;
const TAG_FALSE = 0b10;
const TAG_TRUE = 0b11;

pub const Value = if (NAN_BOXING) NanBoxedValue else UnionValue;

pub const NanBoxedValue = packed struct {
    bits: u64,

    pub fn number(value: f64) Value {
        return Value{ .bits = @bitCast(value) };
    }
    pub fn asNumber(self: Value) f64 {
        return @bitCast(self.bits);
    }
    pub fn isNumber(self: Value) bool {
        return self.bits & QNAN != QNAN;
    }

    pub fn nil() Value {
        return Value{ .bits = QNAN | TAG_NIL };
    }
    pub fn isNil(self: Value) bool {
        return self.bits == nil().bits;
    }

    fn @"false"() Value {
        return Value{ .bits = QNAN | TAG_FALSE };
    }
    fn @"true"() Value {
        return Value{ .bits = QNAN | TAG_TRUE };
    }
    pub fn @"bool"(value: bool) Value {
        return if (value) @"true"() else @"false"();
    }
    pub fn asBool(self: Value) bool {
        return self.bits == @"true"().bits;
    }
    pub fn isBool(self: Value) bool {
        return (self.bits | 1) == @"true"().bits;
    }

    pub fn object(value: *obj.Obj) Value {
        const ptr_int: u64 = @intFromPtr(value);
        return Value{ .bits = SIGN_BIT | QNAN | ptr_int };
    }
    pub fn asObject(self: Value) *obj.Obj {
        return @ptrFromInt(self.bits & ~@as(u64, SIGN_BIT | QNAN));
    }
    pub fn isObject(self: Value) bool {
        return (self.bits & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }
};

pub const UnionValue = union(enum) {
    none,
    boolean: bool,
    num: f64,
    obj: *obj.Obj,

    pub fn number(value: f64) Value {
        // std.debug.print("[value.number]\n", .{});
        return Value{ .num = value };
    }
    pub fn asNumber(self: Value) f64 {
        return switch (self) {
            .num => |n| n,
            else => std.debug.panic("Value is not a number, it's: {any}", .{self}),
        };
    }
    pub fn isNumber(self: Value) bool {
        return self == .num;
    }

    pub fn nil() Value {
        return Value{ .none = {} };
    }
    pub fn isNil(self: Value) bool {
        return self == .none;
    }

    pub fn @"bool"(value: bool) Value {
        return Value{ .boolean = value };
    }
    pub fn asBool(self: Value) bool {
        return switch (self) {
            .boolean => |b| b,
            else => @panic("Value is not a boolean"),
        };
    }
    pub fn isBool(self: Value) bool {
        return self == .boolean;
    }

    pub fn object(value: *obj.Obj) Value {
        return Value{ .obj = value };
    }
    pub fn isObject(self: Value) bool {
        return self == .obj;
    }
    pub fn asObject(self: Value) *obj.Obj {
        return switch (self) {
            .obj => |o| o,
            else => std.debug.panic("Value is not an object, it's: {any}", .{self}),
        };
    }
};

pub const ValueArray = struct {
    values: []Value,
    len: usize,
    capacity: usize,
    vm_allocator: *memory.VMAllocator,

    pub fn init(vm_allocator: *memory.VMAllocator) !ValueArray {
        return ValueArray{
            .values = &[_]Value{},
            .len = 0,
            .capacity = 0,
            .vm_allocator = vm_allocator,
        };
    }
    pub fn write(self: *ValueArray, value: Value) void {
        // std.debug.print("[value.write] writing value: {any}\n", .{value});
        if (self.capacity < self.len + 1) {
            const old_capacity = self.capacity;
            self.capacity = memory.growCapacity(old_capacity);
            self.values = self.vm_allocator.reallocate(
                Value,
                self.values,
                self.capacity,
            );
        }
        self.values[self.len] = value;
        self.len += 1;
    }
    pub fn free(self: *ValueArray) void {
        if (self.capacity == 0) return;
        self.vm_allocator.free(self.values);
        self.values = &[_]Value{};
        self.len = 0;
        self.capacity = 0;
    }
};

pub fn printValue(value: Value, writer: *std.Io.Writer) !void {
    if (NAN_BOXING) {
        if (value.isBool()) {
            try writer.print("{s}", .{if (value.asBool()) "true" else "false"});
        } else if (value.isNil()) {
            try writer.print("nil", .{});
        } else if (value.isNumber()) {
            try writer.print("{d}", .{value.asNumber()});
        } else if (value.isObject()) {
            try printObject(value, writer);
        }
    } else {
        switch (value) {
            .boolean => try writer.print("{s}", .{if (value.asBool()) "true" else "false"}),
            .num => try writer.print("{d}", .{value.asNumber()}),
            .obj => try printObject(value, writer),
            .none => try writer.print("nil", .{}),
        }
    }
}

pub fn toString(value: Value, allocator: *std.mem.Allocator) ![]const u8 {
    return switch (value) {
        .boolean => if (value.asBool()) "true" else "false",
        .num => try std.fmt.allocPrint(allocator.*, "{d}", .{value.asNumber()}),
        .obj => {
            return switch (value.objType()) {
                .string => value.asCString(),
                .function => if (value.asFunction().name) |name| name.chars else "",
                .native => "native func",
                .closure => if (value.asClosure().function.name) |name| name.chars else "",
                .upvalue => "upvalue",
            };
        },
        .none => "nil",
    };
}

fn printObject(value: Value, writer: *std.Io.Writer) !void {
    switch (obj.objType(value)) {
        .string => try writer.print("{s}", .{obj.asCString(value)}),
        .function => {
            const func = obj.asFunction(value);
            try func.print(writer);
        },
        .bound_method => {
            const bound_method = obj.asBoundMethod(value);
            try bound_method.method.function.print(writer);
        },
        .native => try writer.print("<native fn>", .{}),
        .closure => try obj.asClosure(value).function.print(writer),
        .upvalue => try writer.print("upvalue", .{}),
        .class => try writer.print("{s}", .{obj.asClass(value).name.chars}),
        .instance => try writer.print("{s} instance", .{obj.asInstance(value).class.name.chars}),
    }
}

pub fn valuesEqual(a: Value, b: Value) bool {
    if (NAN_BOXING) {
        if (a.isNumber() and b.isNumber()) {
            return a.asNumber() == b.asNumber();
        }
        return a.bits == b.bits;
    }

    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    switch (a) {
        .boolean => return a.asBool() == b.asBool(),
        .none => return true,
        .num => return a.asNumber() == b.asNumber(),
        .obj => return a.asObject() == b.asObject(),
    }
}
