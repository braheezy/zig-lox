const main = @import("main.zig");
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
    };
    main.vm.objects = &obj.obj;
    return obj;
}

// Allocates an ObjString with the provided characters.
pub fn allocateString(chars: []const u8) !*ObjString {
    var string = try allocateObject(ObjString, .string);
    string.chars = chars;
    return string;
}

pub fn takeString(chars: []const u8) !*ObjString {
    return allocateString(chars);
}

// Copies the input string into a new heap allocation and creates an ObjString.
pub fn copyString(chars: []const u8) !*ObjString {
    const length = chars.len;
    const heapChars = try main.allocator.alloc(u8, length);

    @memcpy(heapChars, chars);

    // Create a slice from the allocated buffer
    const charsSlice = heapChars[0..length];

    return allocateString(charsSlice);
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
