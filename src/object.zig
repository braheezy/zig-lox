const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const main = @import("main.zig");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
const Table = @import("table.zig").Table;
pub const ObjType = enum(u8) {
    bound_method,
    class,
    closure,
    function,
    instance,
    native,
    string,
    upvalue,
};

pub const Obj = struct {
    obj_type: ObjType,
    is_marked: bool,
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
    upvalue_count: u8,
};

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
};

pub const ObjClass = struct {
    obj: Obj,
    name: *ObjString,
    methods: *Table,
};

pub const ObjBoundMethod = struct {
    obj: Obj,
    receiver: Value,
    method: *ObjClosure,
};

pub const ObjInstance = struct {
    obj: Obj,
    class: *ObjClass,
    fields: *Table,
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    upvalue_count: u8,
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

pub const NativeFn = *const fn (arg_count: u8, args: []Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,
};

// Allocates an object of a given type.
pub fn allocateObject(vm: *VM, comptime T: type, obj_type: ObjType) std.mem.Allocator.Error!*T {
    // std.debug.print("[allocateObject 1] creating type: {any}\n", .{T});
    const obj = try vm.allocator.create(T);
    switch (T) {
        ObjString => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .hash = 0,
                .chars = undefined,
            };
        },
        ObjFunction => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .arity = 0,
                .name = null,
                .chunk = undefined,
                .upvalue_count = 0,
            };
        },
        ObjNative => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .function = undefined,
            };
        },
        ObjBoundMethod => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .receiver = Value.nil(),
                .method = undefined,
            };
        },
        ObjClosure => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .function = undefined,
                .upvalues = undefined,
                .upvalue_count = 0,
            };
        },
        ObjClass => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .name = undefined,
                .methods = undefined,
            };
        },
        ObjInstance => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .class = undefined,
                .fields = undefined,
            };
        },
        ObjUpvalue => {
            obj.* = T{
                .obj = Obj{
                    .obj_type = obj_type,
                    .next = vm.objects,
                    .is_marked = false,
                },
                .location = undefined,
                .next = null,
                .closed = Value.nil(),
            };
        },
        else => unreachable,
    }

    vm.objects = &obj.obj;

    if (main.DEBUG_LOG_GC) std.debug.print("allocate {d} for {s}\n", .{
        @sizeOf(T),
        @tagName(obj_type),
    });

    return obj;
}

// Allocates an ObjString with the provided characters.
pub fn allocateString(vm: *VM, chars: []const u8, hash: u64) !*ObjString {
    // std.debug.print("[allocateString] {s}\n", .{chars});
    var string = try allocateObject(vm, ObjString, .string);
    string.chars = chars;
    string.hash = hash;
    vm.push(Value.object(&string.obj));
    _ = vm.strings.set(string, Value.nil());
    _ = vm.pop();
    return string;
}

pub fn newFunction(vm: *VM) std.mem.Allocator.Error!*ObjFunction {
    // std.debug.print("[newFunction]\n", .{});
    var func: *ObjFunction = try allocateObject(vm, ObjFunction, .function);
    func.chunk = try Chunk.init(vm.allocator);
    return func;
}

pub fn newClass(vm: *VM, name: *ObjString) !*ObjClass {
    const class: *ObjClass = try allocateObject(vm, ObjClass, .class);
    class.name = name;
    class.methods = try Table.init(vm.allocator);
    return class;
}

pub fn newInstance(vm: *VM, class: *ObjClass) !*ObjInstance {
    const instance: *ObjInstance = try allocateObject(vm, ObjInstance, .instance);
    instance.class = class;
    instance.fields = try Table.init(vm.allocator);
    return instance;
}

pub fn newBoundMethod(vm: *VM, receiver: Value, method: *ObjClosure) !*ObjBoundMethod {
    const bound_method: *ObjBoundMethod = try allocateObject(vm, ObjBoundMethod, .bound_method);
    bound_method.receiver = receiver;
    bound_method.method = method;
    return bound_method;
}

pub fn newNative(vm: *VM, func: NativeFn) std.mem.Allocator.Error!*ObjNative {
    // std.debug.print("[newNative] allocating native\n", .{});
    var native: *ObjNative = try allocateObject(vm, ObjNative, .native);
    native.function = func;
    // std.debug.print("[newNative END]\n", .{});
    return native;
}

pub fn newClosure(vm: *VM, func: *ObjFunction) !*ObjClosure {
    // std.debug.print("[newClosure] \n", .{});
    const upvalues = vm.allocator.reallocate(?*ObjUpvalue, null, func.upvalue_count);
    for (upvalues) |*upvalue| {
        upvalue.* = null;
    }

    var closure: *ObjClosure = try allocateObject(vm, ObjClosure, .closure);
    closure.function = func;
    // std.debug.print("[newClosure] func.upvalue_count: {d}\n", .{func.upvalue_count});
    closure.upvalues = upvalues;
    closure.upvalue_count = func.upvalue_count;

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
    // std.debug.print("[copyString] {s}\n", .{chars});
    const hash = hashString(chars);
    const interned = vm.strings.findString(chars, hash);
    if (interned) |i| return i;
    const length = chars.len;
    const heap_chars = vm.allocator.reallocate(u8, null, length);

    @memcpy(heap_chars, chars);

    return allocateString(vm, heap_chars, hash);
}

pub fn freeObject(vm: *VM, obj: *Obj) void {
    if (main.DEBUG_LOG_GC) std.debug.print("freeing type: {s}\n", .{@tagName(obj.obj_type)});
    switch (obj.obj_type) {
        .string => {
            const obj_string: *ObjString = @ptrCast(obj);
            vm.allocator.free(obj_string.chars);
            vm.allocator.destroy(obj_string);
        },
        .function => {
            const obj_function: *ObjFunction = @ptrCast(obj);
            // std.debug.print("[freeObject] freeing function: {s}\n", .{if (obj_function.name) |name| name.chars else "null-name"});

            obj_function.chunk.free(vm.allocator);
            vm.allocator.destroy(obj_function);
        },
        .native => {
            const obj_native: *ObjNative = @ptrCast(obj);
            vm.allocator.destroy(obj_native);
        },
        .closure => {
            const obj_closure: *ObjClosure = @ptrCast(obj);
            vm.allocator.free(obj_closure.upvalues);
            vm.allocator.destroy(obj_closure);
        },
        .upvalue => {
            const obj_upvalue: *ObjUpvalue = @ptrCast(obj);
            vm.allocator.destroy(obj_upvalue);
        },
        .class => {
            const obj_class: *ObjClass = @ptrCast(obj);
            obj_class.methods.free();
            vm.allocator.destroy(obj_class);
        },
        .bound_method => {
            const obj_bound_method: *ObjBoundMethod = @ptrCast(obj);
            vm.allocator.destroy(obj_bound_method);
        },
        .instance => {
            const obj_instance: *ObjInstance = @ptrCast(obj);
            obj_instance.fields.free();
            vm.allocator.destroy(obj_instance);
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

pub fn isClass(value: Value) bool {
    return isObjType(value, .class);
}

pub fn isInstance(value: Value) bool {
    return isObjType(value, .instance);
}

pub fn isString(value: Value) bool {
    return isObjType(value, .string);
}

pub fn isFunction(value: Value) bool {
    return isObjType(value, .function);
}

pub fn isBoundMethod(value: Value) bool {
    return isObjType(value, .bound_method);
}

pub fn isNative(value: Value) bool {
    return isObjType(value, .native);
}

pub fn isClosure(value: Value) bool {
    return isObjType(value, .closure);
}

pub fn isObjType(value: Value, target_obj_type: ObjType) bool {
    return value.isObject() and value.asObject().obj_type == target_obj_type;
}

pub fn asBoundMethod(value: Value) *ObjBoundMethod {
    return asType(ObjBoundMethod, value);
}

pub fn asClass(value: Value) *ObjClass {
    return asType(ObjClass, value);
}

pub fn asInstance(value: Value) *ObjInstance {
    return asType(ObjInstance, value);
}

pub fn asType(comptime T: type, value: Value) *T {
    const obj_ptr = value.asObject();
    return @alignCast(@fieldParentPtr("obj", obj_ptr));
}

pub fn asFunction(value: Value) *ObjFunction {
    return asType(ObjFunction, value);
}

pub fn asNative(value: Value) NativeFn {
    const result = asType(ObjNative, value);
    return result.function;
}

pub fn asClosure(value: Value) *ObjClosure {
    return asType(ObjClosure, value);
}

pub fn asString(value: Value) *ObjString {
    return asType(ObjString, value);
}

pub fn asCString(value: Value) []const u8 {
    const obj_string = asString(value);
    return obj_string.chars;
}

pub fn objType(value: Value) ObjType {
    return value.asObject().obj_type;
}
