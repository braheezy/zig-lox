const std = @import("std");
pub const Value = union(enum) {
    none,
    boolean: bool,
    number: f64,

    // Constructor methods
    pub fn @"bool"(value: bool) Value {
        return Value{ .boolean = value };
    }

    pub fn number(value: f64) Value {
        return Value{ .number = value };
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

    pub fn isNil(self: Value) bool {
        return self == .none;
    }

    // Accessor methods
    pub fn asBool(self: Value) bool {
        return switch (self) {
            .boolean => |b| b,
            else => @panic("Value is not a boolean"),
        };
    }

    pub fn asNumber(self: Value) f64 {
        return switch (self) {
            .number => |n| n,
            else => @panic("Value is not a number"),
        };
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
        .none => try writer.print("nil", .{}),
    }
}

pub fn valuesEqual(a: Value, b: Value) bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
    switch (a) {
        .boolean => return a.asBool() == b.asBool(),
        .none => return true,
        .number => return a.asNumber() == b.asNumber(),
    }
}
