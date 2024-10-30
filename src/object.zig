const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
pub const ObjType = enum(u8) {
    closure,
    function,
    native,
    string,
    upvalue,
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

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,
    upvalueCount: u8,
};

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    upvalueCount: u8,
    chunk: Chunk,
    name: ?*ObjString,

    pub fn print(self: *ObjFunction, writer: std.fs.File.Writer) !void {
        if (self.name) |name| {
            try writer.print("<fn {s}>", .{name.chars});
        } else {
            try writer.print("<script>", .{});
        }
    }
};

pub const NativeFn = *const fn (argCount: u8, args: []Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,
};

// Allocates an object of a given type.
pub fn allocateObject(vm: *VM, comptime T: type, objType: ObjType) std.mem.Allocator.Error!*T {
    const obj = try vm.allocator.create(T);
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
                .upvalueCount = 0,
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
        ObjClosure => {
            obj.* = T{
                .obj = Obj{
                    .objType = objType,
                    .next = vm.objects,
                },
                .function = undefined,
                .upvalues = undefined,
                .upvalueCount = 0,
            };
        },
        ObjUpvalue => {
            obj.* = T{
                .obj = Obj{
                    .objType = objType,
                    .next = vm.objects,
                },
                .location = undefined,
                .next = null,
                .closed = Value.nil(),
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
    var native: *ObjNative = try allocateObject(vm, ObjNative, .native);
    native.function = func;
    return native;
}

pub fn newClosure(vm: *VM, func: *ObjFunction) !*ObjClosure {
    var closure: *ObjClosure = try allocateObject(vm, ObjClosure, .closure);
    closure.function = func;

    // std.debug.print("[newClosure] func.upvalueCount: {d}\n", .{func.upvalueCount});
    closure.upvalues = try vm.allocator.alloc(?*ObjUpvalue, func.upvalueCount);
    for (closure.upvalues) |*upvalue| {
        upvalue.* = null;
    }
    closure.upvalueCount = func.upvalueCount;

    return closure;
}

pub fn newUpvalue(vm: *VM, slot: *Value) !*ObjUpvalue {
    var upvalue: *ObjUpvalue = try allocateObject(vm, ObjUpvalue, .upvalue);
    upvalue.location = slot;
    return upvalue;
}

pub fn takeString(vm: *VM, chars: []const u8) !*ObjString {
    const hash = hashString(chars);
    const interned = vm.strings.findString(chars, hash);
    if (interned) |i| {
        vm.allocator.free(chars);
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
    const heapChars = try vm.allocator.alloc(u8, length);

    @memcpy(heapChars, chars);

    // Create a slice from the allocated buffer
    const charSlice = heapChars[0..length];

    return allocateString(vm, charSlice, hash);
}

pub fn freeObject(vm: *VM, obj: *Obj) void {
    switch (obj.objType) {
        .string => {
            const objString: *ObjString = @ptrCast(obj);
            vm.allocator.free(objString.chars);
            vm.allocator.destroy(objString);
        },
        .function => {
            const objFunction: *ObjFunction = @ptrCast(obj);
            objFunction.chunk.free(vm.allocator);
            vm.allocator.destroy(objFunction);
        },
        .native => {
            const objNative: *ObjNative = @ptrCast(obj);
            vm.allocator.destroy(objNative);
        },
        .closure => {
            const objClosure: *ObjClosure = @ptrCast(obj);
            vm.allocator.free(objClosure.upvalues);
            vm.allocator.destroy(objClosure);
        },
        .upvalue => {
            const objUpvalue: *ObjUpvalue = @ptrCast(obj);
            vm.allocator.destroy(objUpvalue);
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
