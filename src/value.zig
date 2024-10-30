const std = @import("std");

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

    pub fn isObjType(self: Value, targetObjType: obj.ObjType) bool {
        return self.isObject() and self.asObject().objType == targetObjType;
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
            else => @panic("Value is not an object"),
        };
    }

    pub fn asNumber(self: Value) f64 {
        return switch (self) {
            .number => |n| n,
            else => @panic("Value is not a number"),
        };
    }

    pub fn asType(comptime T: type, self: Value) *T {
        const objPtr = self.asObject();
        return @alignCast(@fieldParentPtr("obj", objPtr));
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
        const objString = self.asString();
        return objString.chars;
    }

    pub fn objType(self: Value) obj.ObjType {
        return self.asObject().objType;
    }
};

pub const ValueArray = struct {
    values: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) ValueArray {
        const array = ValueArray{
            .values = std.ArrayList(Value).init(allocator),
        };

        return array;
    }
    pub fn write(self: *ValueArray, value: Value) void {
        self.values.append(value) catch {
            std.debug.print("Failed to allocate memory", .{});
            std.process.exit(1);
        };
    }
    pub fn free(self: *ValueArray) void {
        self.values.deinit();
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
                    if (func.name) |name| {
                        try writer.print("<fn {s}>", .{name.chars});
                    } else {
                        try writer.print("<script>", .{});
                    }
                },
                .native => try writer.print("<native fn>", .{}),
                .closure => try value.asClosure().function.print(writer),
                .upvalue => try writer.print("upvalue", .{}),
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
