const std = @import("std");

const config = @import("config");

const ch = @import("chunk.zig");
const cmp = @import("compiler.zig");
const InterpretResult = @import("vm.zig").InterpretResult;
const VM = @import("vm.zig").VM;
const print = std.debug.print;
const OpenError = std.fs.Dir.OpenError;
const OpCode = ch.OpCode;

pub const DEBUG_TRACE_EXECUTION = config.debug;
pub const DEBUG_PRINT_CODE = config.debug;
pub const DEBUG_STRESS_GC = config.stress_gc;
const one_mb = 1.049E+6;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub var allocator = gpa.allocator();

pub var vm: *VM = undefined;

pub fn main() !void {
    defer if (gpa.deinit() == .leak) {
        std.process.exit(1);
    };
    const stdout = std.io.getStdOut();
    const writer = stdout.writer();
    vm = try VM.init(&allocator, writer);
    defer vm.free();

    // Read arguments
    const args = std.process.argsAlloc(allocator) catch |err| {
        print("error allocating memory: {s}", .{@errorName(err)});
        std.process.exit(12);
    };
    defer std.process.argsFree(allocator, args);

    var eval_mode = false;
    var path: ?[]u8 = null;

    var i: usize = 1;
    while (i < args.len) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--eval")) {
            eval_mode = true;
        } else if (path == null) {
            path = arg;
        } else {
            print("Usage: zig-lox [--eval] [path]\n", .{});
            std.process.exit(64);
        }
        i += 1;
    }

    if (path) |p| {
        try runFile(p);
    } else if (eval_mode) {
        try repl(writer, true);
    } else {
        // No arguments, run normal REPL
        try repl(writer, false);
    }
}

pub fn repl(writer: std.fs.File.Writer, hide_output: bool) !void {
    const stdin = std.io.getStdIn();
    var buffer: [1024]u8 = undefined;
    while (true) {
        if (!hide_output) {
            try writer.print("> ", .{});
        }

        const line = nextLine(stdin.reader(), &buffer) catch |err| {
            print("error reading next line: {s}", .{@errorName(err)});
            std.process.exit(12);
        } orelse {
            if (!hide_output) {
                try writer.print("\nbye!\n", .{});
            }
            break;
        };

        var sentinel_line = try allocator.allocSentinel(u8, line.len, 0);
        defer allocator.free(sentinel_line);

        // Copy input into sentinel array
        @memcpy(sentinel_line[0..line.len], line);

        _ = try vm.interpret(sentinel_line);
        if (cmp.parser.had_error) {
            // reset error and let user continue to use repl
            cmp.parser.had_error = false;
        }
    }
}

pub fn runFile(path: []u8) !void {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        print("Could not open file {s}: {s}.\n", .{ path, @errorName(err) });
        std.process.exit(74);
    };
    defer file.close();

    const file_bytes: [:0]u8 = file.readToEndAllocOptions(
        allocator,
        one_mb,
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
