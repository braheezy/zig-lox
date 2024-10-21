const std = @import("std");

const memory = @import("memory.zig");
const obj = @import("object.zig");
const Value = @import("value.zig").Value;
const ObjString = obj.ObjString;
const takeString = obj.takeString;
const freeObject = obj.freeObject;
const TABLE_MAX_LOAD = 0.75;
pub const Table = struct {
    count: u32,
    capacity: usize,
    entries: ?[]Entry,
    allocator: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator) *Table {
        var table = Table{
            .count = 0,
            .capacity = 0,
            .entries = null,
            .allocator = allocator,
        };
        return &table;
    }

    pub fn free(self: *Table) void {
        self.count = 0;
        self.capacity = 0;
    }

    pub fn get(self: *Table, key: *ObjString) ?*Value {
        if (self.count == 0) return null;

        const entry = findEntry(self.entries.?, self.capacity, key);
        if (entry) |e| {
            if (e.key) |_| {
                return &e.value;
            }
        }
        return null;
    }

    pub fn set(self: *Table, key: *ObjString, value: Value) !bool {
        const fCap: f64 = @floatFromInt(self.capacity);
        const limit: u32 = @intFromFloat(fCap * TABLE_MAX_LOAD);
        if (self.count + 1 > limit) {
            const new_capacity = memory.growCapacity(self.capacity);
            try self.adjustCapacity(new_capacity);
        }
        if (self.entries) |entries| {
            const entry = findEntry(entries, self.capacity, key);

            if (entry) |e| {
                if (e.value.isNil()) self.count += 1;

                e.key = key;
                e.value = value;
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    pub fn delete(self: *Table, key: *ObjString) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, self.capacity, key);
        if (entry.key) {
            // place a tombstone
            entry.key = null;
            entry.value = Value.bool(true);
            return true;
        } else {
            return false;
        }
    }

    pub fn setAll(from: *Table, to: *Table) void {
        for (0..from.capacity) |i| {
            const entry = &from.entries.?[i];
            if (entry.key) |key| {
                to.set(key, entry.value);
            }
        }
    }

    pub fn findString(self: *Table, chars: []const u8, hash: u64) ?*ObjString {
        if (self.count == 0) return null;

        var index = hash % self.capacity;
        while (true) {
            const entry = &self.entries.?[index];
            if (entry.key) |key| {
                if (key.chars.len == chars.len and key.hash == hash and std.mem.eql(u8, key.chars, chars)) {
                    // found it
                    return key;
                }
            } else {
                // stop if we find an empty non-tombstone entry
                if (entry.value.isNil()) return null;
            }

            index = (index + 1) % self.capacity;
        }
    }

    fn findEntry(entries: []Entry, capacity: usize, key: *ObjString) ?*Entry {
        var index = key.hash % capacity;
        var tombstone: ?*Entry = null;
        while (true) {
            const entry = &entries[index];
            if (entry.key == null) {
                if (entry.value.isNil()) {
                    // empty entry
                    return tombstone;
                } else {
                    // found tombstone
                    if (tombstone == null) tombstone = entry;
                }
            } else if (entry.key == key) {
                // found the key
                return entry;
            }
            index = (index + 1) % capacity;
        }
    }

    fn adjustCapacity(self: *Table, capacity: usize) !void {
        var entries = try self.allocator.alloc(Entry, capacity);
        for (0..capacity) |i| {
            entries[i].key = null;
            entries[i].value = Value.nil();
        }

        self.count = 0;
        for (0..self.capacity) |i| {
            const entry = &self.entries.?[i];
            if (entry.key) |key| {
                var dest = findEntry(entries, capacity, key);
                if (dest != null) {
                    dest.?.key = key;
                    dest.?.value = entry.value;
                    self.count += 1;
                }
            }
        }

        self.allocator.free(self.entries.?);
        self.entries = entries;
        self.capacity = capacity;
    }
};

const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

test "Table initialization" {
    var allocator = std.testing.allocator;
    const table = Table.init(&allocator);
    defer table.free();

    try std.testing.expectEqual(@as(u32, 0), table.count);
    try std.testing.expectEqual(@as(u32, 0), table.capacity);
    try std.testing.expect(table.entries == null);
}
