const std = @import("std");
pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}
pub fn growArray(
    comptime T: type,
    allocator: *std.mem.Allocator,
    oldArray: []T,
    newCount: usize,
) []T {
    const newArray = allocator.realloc(oldArray, newCount) catch {
        std.debug.print("Failed to allocate memory", .{});
        std.process.exit(1);
    };
    return newArray;
}
