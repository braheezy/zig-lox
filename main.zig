const std = @import("std");

const config = @import("config");

const ch = @import("chunk.zig");
const InterpretResult = @import("vm.zig").InterpretResult;
const VM = @import("vm.zig").VM;
const print = std.debug.print;
const OpenError = std.fs.Dir.OpenError;
const OpCode = ch.OpCode;

pub const DEBUG_TRACE_EXECUTION = config.debug;
pub const DEBUG_PRINT_CODE = config.debug;
const oneMB = 1.049E+6;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub var allocator = gpa.allocator();

var vm: VM = undefined;

pub fn main() !void {
    defer if (gpa.deinit() == .leak) {
        std.process.exit(1);
    };
    vm = VM.init(&allocator);
    defer vm.free(&allocator);

    // Read arguments
    const args = std.process.argsAlloc(allocator) catch |err| {
        print("error allocating memory: {s}", .{@errorName(err)});
        std.process.exit(12);
    };
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        try repl();
    } else if (args.len == 2) {
        try runFile(args[1]);
    } else {
        print("Usage: zig-lox [path]\n", .{});
        std.process.exit(64);
    }
}

pub fn repl() !void {
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

        var sentinel_line = try allocator.allocSentinel(u8, line.len, 0);
        defer allocator.free(sentinel_line);

        // Copy input into sentinel array
        @memcpy(sentinel_line[0..line.len], line);

        _ = try vm.interpret(sentinel_line);
    }
}

pub fn runFile(path: []u8) !void {
    print("running file: {s}\n", .{path});
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        print("Could not open file {s}: {s}.\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };
    defer file.close();

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
    defer allocator.free(file_bytes);
    const result = try vm.interpret(file_bytes);

    if (result == InterpretResult.INTERPRET_COMPILE_ERROR) std.process.exit(65);
    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) std.process.exit(70);
}

fn nextLine(reader: anytype, buffer: []u8) !?[]u8 {
    var line = (try reader.readUntilDelimiterOrEof(
        buffer,
        '\n',
    )) orelse return null;
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        line = std.mem.trimRight(u8, line, "\r");
    }

    return line;
}
