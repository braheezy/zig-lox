const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const main = @import("main.zig");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
pub const ObjType = enum(u8) {
    function,
    native,
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

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ?*ObjString,
};

pub const NativeFn = *const fn (argCount: u8, args: []Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,
};

// Allocates an object of a given type.
pub fn allocateObject(vm: *VM, comptime T: type, objType: ObjType) std.mem.Allocator.Error!*T {
    const obj = try main.allocator.create(T);
    switch (T) {
        ObjString => {
            obj.* = T{
                .obj = Obj{
                    .objType = objType,
                    .next = vm.objects,
                },
                .hash = 0,
                .chars = undefined,
            };
        },
        ObjFunction => {
            obj.* = T{
                .obj = Obj{
                    .objType = objType,
                    .next = vm.objects,
                },
                .arity = 0,
                .name = null,
                .chunk = undefined,
            };
        },
        ObjNative => {
            obj.* = T{
                .obj = Obj{
                    .objType = objType,
                    .next = vm.objects,
                },
                .function = undefined,
            };
        },
        else => unreachable,
    }

    vm.objects = &obj.obj;
    return obj;
}

// Allocates an ObjString with the provided characters.
pub fn allocateString(vm: *VM, chars: []const u8, hash: u64) !*ObjString {
    var string = try allocateObject(vm, ObjString, .string);
    string.chars = chars;
    string.hash = hash;
    _ = try vm.strings.set(string, Value.nil());
    return string;
}

pub fn newFunction(vm: *VM, allocator: *std.mem.Allocator) std.mem.Allocator.Error!*ObjFunction {
    var func: *ObjFunction = try allocateObject(vm, ObjFunction, .function);
    func.chunk = try Chunk.init(allocator);
    return func;
}

pub fn newNative(vm: *VM, func: NativeFn) std.mem.Allocator.Error!*ObjNative {
    var native = try allocateObject(vm, ObjNative, .native);
    native.function = func;
    return native;
}

pub fn takeString(vm: *VM, chars: []const u8) !*ObjString {
    const hash = hashString(chars);
    const interned = main.vm.strings.findString(chars, hash);
    if (interned) |i| {
        main.allocator.free(chars);
        return i;
    }
    return allocateString(vm, chars, hash);
}

// Copies the input string into a new heap allocation and creates an ObjString.
pub fn copyString(vm: *VM, chars: []const u8) !*ObjString {
    const hash = hashString(chars);
    const interned = vm.strings.findString(chars, hash);
    if (interned) |i| return i;
    const length = chars.len;
    const heapChars = try main.allocator.alloc(u8, length);

    @memcpy(heapChars, chars);

    // Create a slice from the allocated buffer
    const charSlice = heapChars[0..length];

    return allocateString(vm, charSlice, hash);
}

pub fn freeObject(obj: *Obj) void {
    switch (obj.objType) {
        .string => {
            const objString: *ObjString = @ptrCast(obj);
            main.allocator.free(objString.chars);
            main.allocator.destroy(objString);
        },
        .function => {
            const func: *ObjFunction = @ptrCast(obj);
            func.chunk.free(&main.allocator);
            main.allocator.destroy(func);
        },
        .native => {
            const func: *ObjNative = @ptrCast(obj);
            main.allocator.destroy(func);
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
