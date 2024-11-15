const std = @import("std");

const memory = @import("memory.zig");
const obj = @import("object.zig");
pub const Value = union(enum) {
    none,
    boolean: bool,
    number: f64,
    object: *obj.Obj,

    // Constructor methods
    pub fn @"bool"(value: bool) Value {
        return Value{ .boolean = value };
    }

    pub fn number(value: f64) Value {
        // std.debug.print("[value.number]\n", .{});
        return Value{ .number = value };
    }

    pub fn object(value: *obj.Obj) Value {
        return Value{ .object = value };
    }

    pub fn nil() Value {
        return Value{ .none = {} };
    }

    // Type check methods
    pub fn isBool(self: Value) bool {
        return self == .boolean;
    }

    pub fn isNumber(self: Value) bool {
        return self == .number;
    }

    pub fn isObject(self: Value) bool {
        return self == .object;
    }

    pub fn isNil(self: Value) bool {
        return self == .none;
    }

    pub fn isClass(self: Value) bool {
        return self.isObjType(.class);
    }

    pub fn isInstance(self: Value) bool {
        return self.isObjType(.instance);
    }

    pub fn isString(self: Value) bool {
        return self.isObjType(.string);
    }

    pub fn isFunction(self: Value) bool {
        return self.isObjType(.function);
    }

    pub fn isNative(self: Value) bool {
        return self.isObjType(.native);
    }

    pub fn isClosure(self: Value) bool {
        return self.isObjType(.closure);
    }

    pub fn isObjType(self: Value, target_obj_type: obj.ObjType) bool {
        return self.isObject() and self.asObject().obj_type == target_obj_type;
    }

    // Accessor methods
    pub fn asBool(self: Value) bool {
        return switch (self) {
            .boolean => |b| b,
            else => @panic("Value is not a boolean"),
        };
    }
    pub fn asObject(self: Value) *obj.Obj {
        return switch (self) {
            .object => |o| o,
            else => std.debug.panic("Value is not an object, it's: {any}", .{self}),
        };
    }

    pub fn asNumber(self: Value) f64 {
        return switch (self) {
            .number => |n| n,
            else => std.debug.panic("Value is not a number, it's: {any}", .{self}),
        };
    }

    pub fn asClass(self: Value) *obj.ObjClass {
        return asType(obj.ObjClass, self);
    }

    pub fn asInstance(self: Value) *obj.ObjInstance {
        return asType(obj.ObjInstance, self);
    }

    pub fn asType(comptime T: type, self: Value) *T {
        const obj_ptr = self.asObject();
        return @alignCast(@fieldParentPtr("obj", obj_ptr));
    }

    pub fn asFunction(self: Value) *obj.ObjFunction {
        return asType(obj.ObjFunction, self);
    }

    pub fn asNative(self: Value) obj.NativeFn {
        const result = asType(obj.ObjNative, self);
        return result.function;
    }

    pub fn asClosure(self: Value) *obj.ObjClosure {
        return asType(obj.ObjClosure, self);
    }

    pub fn asString(self: Value) *obj.ObjString {
        return asType(obj.ObjString, self);
    }

    pub fn asCString(self: Value) []const u8 {
        const obj_string = self.asString();
        return obj_string.chars;
    }

    pub fn objType(self: Value) obj.ObjType {
        return self.asObject().obj_type;
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
        // _ = self;
    }
};

pub fn printValue(value: Value, writer: std.fs.File.Writer) !void {
    switch (value) {
        .boolean => try writer.print("{s}", .{if (value.asBool()) "true" else "false"}),
        .number => try writer.print("{d}", .{value.asNumber()}),
        .object => {
            switch (value.objType()) {
                .string => try writer.print("{s}", .{value.asCString()}),
                .function => {
                    const func = value.asFunction();
                    try func.print(writer);
                },
                .native => try writer.print("<native fn>", .{}),
                .closure => try value.asClosure().function.print(writer),
                .upvalue => try writer.print("upvalue", .{}),
                .class => try writer.print("{s}", .{value.asClass().name.chars}),
                .instance => try writer.print("{s} instance", .{value.asInstance().class.name.chars}),
            }
        },
        .none => try writer.print("nil", .{}),
    }
}

pub fn toString(value: Value, allocator: *std.mem.Allocator) ![]const u8 {
    return switch (value) {
        .boolean => if (value.asBool()) "true" else "false",
        .number => try std.fmt.allocPrint(allocator.*, "{d}", .{value.asNumber()}),
        .object => {
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

pub fn valuesEqual(a: Value, b: Value) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    switch (a) {
        .boolean => return a.asBool() == b.asBool(),
        .none => return true,
        .number => return a.asNumber() == b.asNumber(),
        .object => return a.asObject() == b.asObject(),
    }
}
