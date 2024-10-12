const std = @import("std");

const config = @import("config");


const ch = @import("chunk.zig");
const InterpretResult = @import("vm.zig").InterpretResult;
const VM = @import("vm.zig").VM;
const print = std.debug.print;
const OpenError = std.fs.Dir.OpenError;
const OpCode = ch.OpCode;

pub const DEBUG_TRACE_EXECUTION = config.debug;
const oneMB = 1.049E+6;

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    var vm = VM.init(&allocator);
    defer vm.free(&allocator);

    // Read arguments
    const args = std.process.argsAlloc(allocator) catch |err| {
        print("error allocating memory: {s}", .{@errorName(err)});
        std.process.exit(12);
    };
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        repl(&vm);
    } else if (args.len == 2) {
        runFile(&vm, args[1]);
    } else {
        print("Usage: zig-lox [path]\n", .{});
        std.process.exit(64);
    }
}

pub fn repl(vm: *VM) void {
    const stdin = std.io.getStdIn();
    var buffer: [1024]u8 = undefined;
    while (true) {
        print("> ", .{});

        const line = nextLine(stdin.reader(), &buffer) catch |err| {
            print("error reading next line: {s}", .{@errorName(err)});
            std.process.exit(12);
        } orelse {
            print("\nbye!\n", .{});
            break;
        };

        _ = vm.interpret(line);
    }
}

pub fn runFile(vm: *VM, path: []u8) void {
    print("running file: {s}\n", .{path});
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        print("Could not open file {s}: {s}.\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };
    defer file.close();

    const allocator = std.heap.page_allocator;
    const file_bytes: [:0]u8 = file.readToEndAllocOptions(
        allocator,
        oneMB,
        0,
        @alignOf(u8),
        0,
    ) catch |err| {
        print("Failed to read file {s}: {s}.\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };
    const result = vm.interpret(file_bytes);

    if (result == InterpretResult.INTERPRET_COMPILE_ERROR) std.process.exit(65);
    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) std.process.exit(70);
}

fn nextLine(reader: anytype, buffer: []u8) !?[:0]u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        line = std.mem.trimRight(u8, line, "\r");
    }

    line[line.len] = 0;
    return line[0.. :0];
}
