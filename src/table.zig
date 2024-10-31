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

    pub fn init(allocator: *std.mem.Allocator) !*Table {
        const table = try allocator.create(Table);
        table.* = Table{
            .count = 0,
            .capacity = 0,
            .entries = null,
            .allocator = allocator,
        };
        return table;
    }

    pub fn free(self: *Table) void {
        if (self.entries) |entries| {
            self.allocator.free(entries);
            self.entries = null;
        }
        self.allocator.destroy(self);
    }

    pub fn get(self: *Table, key: *ObjString) ?*Value {
        if (self.count == 0) return null;

        if (self.entries == null) return null;

        const entry = findEntry(self.entries.?, self.capacity, key) orelse return null;
        if (entry.key == null) return null;

        return &entry.value;
    }

    pub fn set(self: *Table, key: *ObjString, value: Value) !bool {
        const f_cap: f64 = @floatFromInt(self.capacity);
        const limit: u32 = @intFromFloat(f_cap * TABLE_MAX_LOAD);
        var is_new_key = false;
        if (self.count + 1 > limit) {
            const new_capacity = memory.growCapacity(self.capacity);
            try self.adjustCapacity(new_capacity);
        }
        if (self.entries) |entries| {
            const entry = findEntry(entries, self.capacity, key);

            if (entry) |e| {
                is_new_key = (e.key == null);
                if (is_new_key and e.value.isNil()) {
                    self.count += 1;
                }

                e.key = key;
                e.value = value;
                return is_new_key;
            } else {
                return is_new_key;
            }
        }
        return is_new_key;
    }

    pub fn delete(self: *Table, key: *ObjString) bool {
        if (self.count == 0) return false;

        if (self.entries) |entries| {
            const entry = findEntry(entries, self.capacity, key);
            if (entry) |e| {
                if (e.key) |_| {
                    // place a tombstone
                    e.key = null;
                    e.value = Value.bool(true);
                    return true;
                } else {
                    return false;
                }
            }
        }
        return false;
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
            if (self.entries) |entries| {
                const entry = &entries[index];
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
            } else {
                return null;
            }
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
                    if (tombstone != null) {
                        return tombstone;
                    } else {
                        return entry;
                    }
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

        if (self.entries) |ents| self.allocator.free(ents);
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
    const table = try Table.init(&allocator);
    defer table.free();

    try std.testing.expectEqual(@as(u32, 0), table.count);
    try std.testing.expectEqual(@as(u32, 0), table.capacity);
    try std.testing.expect(table.entries == null);
}
