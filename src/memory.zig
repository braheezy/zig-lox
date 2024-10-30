const std = @import("std");
pub const VMAllocator = struct {
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) VMAllocator {
        return VMAllocator{
            .allocator = allocator,
        };
    }

    pub fn reallocate(
        self: *VMAllocator,
        comptime T: type,
        old_mem: ?[]T,
        new_size: usize,
    ) []T {
        // TODO: Cleanup
        var old_size: usize = 0;
        if (old_mem) |mem| old_size = mem.len;

        if (old_mem) |mem| {
            // std.debug.print(" --> realloc\n", .{});
            const new_array = self.allocator.realloc(mem, new_size) catch {
                std.debug.print("Failed to allocate memory!", .{});
                std.process.exit(1);
            };
            return new_array;
        } else {
            // std.debug.print(" --> alloc\n", .{});
            const new_array = self.allocator.alloc(T, new_size) catch {
                std.debug.print("Failed to allocate memory!", .{});
                std.process.exit(1);
            };
            return new_array;
        }
    }

    pub fn growArray(
        self: *VMAllocator,
        comptime T: type,
        old_array: []T,
        new_count: usize,
    ) []T {
        // std.debug.print("[growArray] {any}\n", .{T});
        const new_array = self.reallocate(
            T,
            old_array,
            new_count,
        );
        return new_array;
    }

    pub fn create(self: *VMAllocator, comptime T: type) !*T {
        // std.debug.print("[create] {any}\n", .{T});
        const obj = try self.allocator.create(T);
        return obj;
    }

    pub fn alloc(self: *VMAllocator, comptime T: type, n: usize) ![]T {
        // std.debug.print("[alloc] {any} of size {d}\n", .{ T, n });
        return try self.allocator.alloc(T, n);
    }

    pub fn free(self: *VMAllocator, memory: anytype) void {
        // std.debug.print("[free] \n", .{});
        self.allocator.free(memory);
    }

    pub fn destroy(self: *VMAllocator, ptr: anytype) void {
        // std.debug.print("[destroy] \n", .{});
        return self.allocator.destroy(ptr);
    }
};

pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

// pub fn collectGarbage() !void {}
