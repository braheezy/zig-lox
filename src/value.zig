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

    pub fn asString(self: Value) *obj.ObjString {
        const objPtr = self.asObject();
        return @alignCast(@fieldParentPtr("obj", objPtr));
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

    pub fn init(allocator: *std.mem.Allocator) ValueArray {
        const array = ValueArray{
            .values = std.ArrayList(Value).init(allocator.*),
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
            }
        },
        .none => try writer.print("nil", .{}),
    }
}

pub fn valuesEqual(a: Value, b: Value) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    switch (a) {
        .boolean => return a.asBool() == b.asBool(),
        .none => return true,
        .number => return a.asNumber() == b.asNumber(),
        .object => {
            const aString = a.asString();
            const bString = b.asString();
            return aString.chars.len == bString.chars.len and std.mem.eql(u8, aString.chars, bString.chars);
        },
    }
}
