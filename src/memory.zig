const std = @import("std");

const markCompilerRoots = @import("compiler.zig").markCompilerRoots;
const main = @import("main.zig");
const Obj = @import("object.zig").Obj;
const ObjClosure = @import("object.zig").ObjClosure;
const ObjFunction = @import("object.zig").ObjFunction;
const ObjUpvalue = @import("object.zig").ObjUpvalue;
const freeObject = @import("object.zig").freeObject;
const Table = @import("table.zig").Table;
const Value = @import("value.zig").Value;
const ValueArray = @import("value.zig").ValueArray;
const printValue = @import("value.zig").printValue;
const VM = @import("vm.zig").VM;
pub const VMAllocator = struct {
    allocator: *std.mem.Allocator,
    vm: ?*VM,

    pub fn init(allocator: *std.mem.Allocator) VMAllocator {
        return VMAllocator{
            .allocator = allocator,
            .vm = null,
        };
    }

    pub fn reallocate(
        self: *VMAllocator,
        comptime T: type,
        old_mem: ?[]T,
        new_size: usize,
    ) []T {
        // if (new_size != 0) if (main.DEBUG_STRESS_GC) self.collectGarbage();

        if (old_mem) |mem| {
            // std.debug.print("[reallocate] --> realloc\n", .{});
            const new_array = self.allocator.realloc(mem, new_size) catch {
                std.debug.print("Failed to allocate memory!", .{});
                std.process.exit(1);
            };
            return new_array;
        } else {
            // std.debug.print("[reallocate] --> alloc\n", .{});
            if (main.DEBUG_STRESS_GC) self.collectGarbage();
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
        const new_array = self.reallocate(
            T,
            old_array,
            new_count,
        );
        return new_array;
    }

    pub fn create(self: *VMAllocator, comptime T: type) !*T {
        // std.debug.print("[create] {any}\n", .{T});
        if (main.DEBUG_STRESS_GC) self.collectGarbage();

        const obj = try self.allocator.create(T);
        return obj;
    }

    pub fn free(self: *VMAllocator, memory: anytype) void {
        // std.debug.print("[free] \n", .{});
        self.allocator.free(memory);
    }

    pub fn destroy(self: *VMAllocator, ptr: anytype) void {
        // std.debug.print("[destroy] \n", .{});
        return self.allocator.destroy(ptr);
    }

    pub fn collectGarbage(self: *VMAllocator) void {
        if (main.DEBUG_LOG_GC) std.debug.print("-- gc begin\n", .{});

        self.markRoots();
        // if (self.vm) |vm| std.debug.print("[collectGarbage] gray stack after marking: {any}\n", .{vm.gray_stack});
        self.traceReferences();
        if (self.vm) |vm| {
            // std.debug.print("[collectGarbage] self.vm.?.strings.capacity: {d}\n", .{self.vm.?.strings.capacity});
            vm.strings.removeWhite();
        }
        self.sweep();

        if (main.DEBUG_LOG_GC) std.debug.print("-- gc end\n", .{});
    }

    fn traceReferences(self: *VMAllocator) void {
        if (self.vm) |vm| {
            // std.debug.print("[traceReferences] vm.gray_count: {d}\n", .{vm.gray_count});
            while (vm.gray_count > 0) {
                vm.gray_count -= 1;
                const object = vm.gray_stack[vm.gray_count];
                if (object) |obj| {
                    self.blackenObject(obj);
                }
            }
        }
    }

    fn sweep(self: *VMAllocator) void {
        if (self.vm) |vm| {
            var previous: ?*Obj = null;
            var object = vm.objects;

            while (object) |obj| {
                // std.debug.print("[sweep] checking \n", .{});
                const current = obj;

                if (current.is_marked) {
                    current.is_marked = false;

                    previous = object;
                    object = current.next;
                } else {
                    const unreached = current;
                    object = current.next;
                    if (previous) |prev| {
                        prev.next = object;
                    } else {
                        vm.objects = object;
                    }
                    // std.debug.print("[sweep] freeing: {any}\n", .{unreached});
                    freeObject(vm, unreached);
                }
            }
        }
    }

    fn blackenObject(self: *VMAllocator, object: *Obj) void {
        if (main.DEBUG_LOG_GC) {
            std.debug.print("{s} blacken ", .{@tagName(object.obj_type)});
            printValue(Value.object(object), std.io.getStdErr().writer()) catch unreachable;
            std.debug.print("\n", .{});
        }
        // std.debug.print("[blackenObject] \n", .{});
        // std.debug.print("[blackenObject] blacken type: {s}\n", .{@tagName(object.obj_type)});
        switch (object.obj_type) {
            .native => {},
            .string => {},
            .upvalue => {
                const obj: *ObjUpvalue = @ptrCast(object);
                self.markValue(obj.closed);
            },
            .function => {
                const function: *ObjFunction = @ptrCast(object);
                if (function.name) |name| {
                    // std.debug.print("[blackenObject] marking function: {s}\n", .{name.chars});
                    self.markObject(&name.obj);
                }
                self.markArray(&function.chunk.constants);
            },
            .closure => {
                const closure: *ObjClosure = @ptrCast(object);

                // std.debug.print("[blackenObject] marking closure: {s}\n", .{closure.function.name.?.chars});
                self.markObject(&closure.function.obj);
                for (0..closure.upvalue_count) |i| {
                    if (closure.upvalues[i]) |upvalue| {
                        self.markObject(&upvalue.obj);
                    }
                }
            },
        }
    }

    fn markRoots(self: *VMAllocator) void {
        if (self.vm) |vm| {
            // std.debug.print("[markRoots] vm.stack_top: {d}\n", .{vm.stack_top});
            // if (vm.stack_top > 0) std.debug.print("[markRoots] vm.stack[0]: {any}\n", .{vm.stack[0]});
            for (vm.stack[0..vm.stack_top]) |slot| {
                // std.debug.print("[markRoots] marking value: {any}\n", .{slot});
                self.markValue(slot);
            }
            for (0..vm.frame_count) |i| {
                self.markObject(&vm.frames[i].closure.obj);
            }
            var upvalue = vm.open_upvalues;
            while (upvalue) |_| {
                self.markObject(&upvalue.?.obj);
                upvalue = upvalue.?.next;
            }
            self.markTable(vm.globals);
            markCompilerRoots();
        }
    }

    fn markArray(self: *VMAllocator, array: *ValueArray) void {
        for (0..array.len) |i| {
            self.markValue(array.values[i]);
        }
    }

    pub fn markObject(self: *VMAllocator, obj: ?*Obj) void {
        // std.debug.print("[markObject] \n", .{});
        if (obj) |object| {
            // std.debug.print("[markObject] object.is_marked {any}\n", .{object.is_marked});
            if (object.is_marked) return;

            if (main.DEBUG_LOG_GC) {
                std.debug.print("{s} mark ", .{@tagName(object.obj_type)});
                // std.debug.print("[markObject] MARK ", .{});
                printValue(Value.object(object), std.io.getStdErr().writer()) catch unreachable;
                std.debug.print("\n", .{});
            }
            object.is_marked = true;

            if (self.vm) |vm| {
                if (vm.gray_capacity < vm.gray_count + 1) {
                    vm.gray_capacity = growCapacity(vm.gray_capacity);
                    vm.gray_stack = self.allocator.realloc(vm.gray_stack, vm.gray_capacity) catch {
                        std.debug.print("Faild to allocate memory for gray stack\n", .{});
                        std.process.exit(1);
                    };
                }
                // std.debug.print("[markObject] adding to grey stack: {any}\n", .{obj.?});
                vm.gray_stack[vm.gray_count] = object;
                vm.gray_count += 1;
            }
        }
    }

    fn markTable(self: *VMAllocator, table: *Table) void {
        // std.debug.print("[markTable] table.capacity: {d}\n", .{table.capacity});
        for (0..table.capacity) |i| {
            const entry = &table.entries.?[i];
            if (entry.key) |key| {
                self.markObject(&key.obj);
            }
            self.markValue(entry.value);
        }
    }

    fn markValue(self: *VMAllocator, value: Value) void {
        if (value.isObject()) {
            // std.debug.print("[markValue] marking value: {any}\n", .{value});
            self.markObject(value.asObject());
        }
    }
};
pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}
