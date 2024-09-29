const std = @import("std");

pub const Value = f64;
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

pub fn printValue(value: Value) void {
    std.debug.print("{d}", .{value});
}
