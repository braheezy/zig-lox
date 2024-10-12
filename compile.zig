const std = @import("std");

const Scanner = @import("scan.zig").Scanner;
const TokenType = @import("scan.zig").TokenType;
const print = std.debug.print;

pub fn compile(source: [:0]u8) void {
    var scanner = Scanner.init(source);
    var line: u32 = 0;
    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            print("{d:4} ", .{token.line});
            line = token.line;
        } else {
            print("   | ", .{});
        }
        print("{d:2} '{s}'\n", .{ @intFromEnum(token.tokenType), token.start });

        if (token.tokenType == TokenType.EOF) break;
    }
}
