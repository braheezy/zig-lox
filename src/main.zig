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
pub const DEBUG_LOG_GC = config.log_gc;
pub const NAN_BOXING = config.nan_boxing;
const one_mb = 1.049E+6;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
pub var allocator = gpa.allocator();

pub var vm: *VM = undefined;

var stdout_buffer: [1024]u8 = undefined;
var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
const stdout = &stdout_writer.interface;

var stdin_buffer: [1024]u8 = undefined;
var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
const stdin = &stdin_reader.interface;

pub fn main() !void {
    defer if (gpa.deinit() == .leak) {
        std.process.exit(1);
    };
    vm = try VM.init(&allocator, stdout);
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
        try repl(stdout, true);
    } else {
        // No arguments, run normal REPL
        try repl(stdout, false);
    }
}

pub fn repl(writer: *std.Io.Writer, hide_output: bool) !void {
    while (true) {
        if (!hide_output) {
            try writer.print("> ", .{});
        }

        const line = nextLine(stdin) catch |err| {
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

    var buffer: [32]u8 = undefined;
    var file_reader = std.fs.File.reader(file, &buffer);
    const file_size = try file_reader.getSize();
    const file_bytes: [:0]u8 = try allocator.allocSentinel(u8, file_size, 0);
    defer allocator.free(file_bytes);
    _ = try file_reader.read(file_bytes);

    const result = try vm.interpret(file_bytes);

    if (result == InterpretResult.INTERPRET_COMPILE_ERROR) std.process.exit(65);
    if (result == InterpretResult.INTERPRET_RUNTIME_ERROR) std.process.exit(70);
}

fn nextLine(reader: *std.Io.Reader) !?[]u8 {
    var line = reader.takeDelimiterExclusive(
        '\n',
    ) catch |err| switch (err) {
        error.EndOfStream => return null,
        else => return err,
    };
    // trim annoying windows-only carriage return character
    if (@import("builtin").os.tag == .windows) {
        line = std.mem.trimRight(u8, line, "\r");
    }

    return line;
}
