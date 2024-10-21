const main = @import("main.zig");
const Value = @import("value.zig").Value;
pub const ObjType = enum(u8) {
    string,
};

pub const Obj = struct {
    objType: ObjType,
    next: ?*Obj,
};

pub const ObjString = struct {
    obj: Obj,
    chars: []const u8,
    hash: u64,
};

// Allocates an object of a given type.
pub fn allocateObject(comptime T: type, objType: ObjType) !*T {
    const obj = try main.allocator.create(T);
    obj.* = T{
        .obj = Obj{
            .objType = objType,
            .next = main.vm.objects,
        },
        .chars = undefined,
        .hash = 0,
    };
    main.vm.objects = &obj.obj;
    return obj;
}

// Allocates an ObjString with the provided characters.
pub fn allocateString(chars: []const u8, hash: u64) !*ObjString {
    var string = try allocateObject(ObjString, .string);
    string.chars = chars;
    string.hash = hash;
    _ = try main.vm.strings.set(string, Value.nil());
    return string;
}

pub fn takeString(chars: []const u8) !*ObjString {
    const hash = hashString(chars);
    const interned = main.vm.strings.findString(chars, hash);
    if (interned) |i| {
        main.allocator.free(chars);
        return i;
    }
    return allocateString(chars, hash);
}

// Copies the input string into a new heap allocation and creates an ObjString.
pub fn copyString(chars: []const u8) !*ObjString {
    const hash = hashString(chars);
    const interned = main.vm.strings.findString(chars, hash);
    if (interned) |i| return i;
    const length = chars.len;
    const heapChars = try main.allocator.alloc(u8, length);

    @memcpy(heapChars, chars);

    // Create a slice from the allocated buffer
    const charSlice = heapChars[0..length];

    return allocateString(charSlice, hash);
}

pub fn freeObject(obj: *Obj) void {
    switch (obj.objType) {
        .string => {
            const objString: *ObjString = @ptrCast(obj);
            main.allocator.free(objString.chars);
            main.allocator.destroy(objString);
        },
    }
}

fn hashString(key: []const u8) u64 {
    var hash: u64 = 2166136261;
    for (key) |byte| {
        hash = (hash ^ byte) *% 16777619;
    }
    return hash;
}
