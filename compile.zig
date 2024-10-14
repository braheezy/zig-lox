const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const main = @import("main.zig");
const DEBUG_PRINT_CODE = @import("main.zig").DEBUG_PRINT_CODE;
const s = @import("scan.zig");
const Token = @import("scan.zig").Token;
const TokenType = @import("scan.zig").TokenType;
const Value = @import("value.zig").Value;
const print = std.debug.print;
var parser: Parser = Parser{ .current = undefined, .previous = undefined, .hadError = false, .panicMode = false };
var compilingChunk: *Chunk = undefined;

const Precedence = enum(u8) {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,

    fn next(current: Precedence) Precedence {
        return @as(Precedence, @enumFromInt(@intFromEnum(current) + 1));
    }
};

const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = s.scanner.scanToken();
            if (self.current.tokenType != TokenType.ERROR) break;

            self.errorAtCurrent(self.current.start);
        }
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) void {
        self.errorAt(&self.current, message);
    }

    fn err(self: *Parser, message: []const u8) void {
        self.errorAt(&self.previous, message);
    }

    fn errorAt(self: *Parser, token: *Token, message: []const u8) void {
        if (self.panicMode) return;
        self.panicMode = true;
        std.debug.print("[line {d}] Error", .{token.line});

        if (token.tokenType == TokenType.EOF) {
            std.debug.print(" at end", .{});
        } else if (token.tokenType == TokenType.ERROR) {
            // nothing
        } else {
            std.debug.print(" at {s}", .{token.start});
        }

        std.debug.print(": {s}\n", .{message});
        self.hadError = true;
    }

    fn consume(self: *Parser, tokenType: TokenType, message: []const u8) void {
        if (self.current.tokenType == tokenType) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn expression(self: *Parser) void {
        self.parsePrecedence(Precedence.ASSIGNMENT);
    }

    fn number(self: *Parser) void {
        const value = std.fmt.parseFloat(f64, self.previous.start) catch 0.0;
        emitConstant(value);
    }

    fn unary(self: *Parser) void {
        const operatorType = self.previous.tokenType;

        // compile the operand
        self.parsePrecedence(Precedence.UNARY);

        // emit the operator instruction
        switch (operatorType) {
            TokenType.MINUS => emitByte(@intFromEnum(OpCode.NEGATE)),
            else => return,
        }
    }

    fn grouping(self: *Parser) void {
        self.expression();
        self.consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        var rule = getRule(self.previous.tokenType);
        if (rule.prefix) |prefixRule| {
            prefixRule(self);
        } else {
            self.err("Expect expression.");
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.tokenType).precedence)) {
            self.advance();
            rule = getRule(self.previous.tokenType);
            if (rule.infix) |infixRule| {
                infixRule(self);
            }
        }
    }

    fn binary(self: *Parser) void {
        const operatorType = self.previous.tokenType;
        const rule = getRule(operatorType);
        self.parsePrecedence(rule.precedence.next());

        switch (operatorType) {
            TokenType.PLUS => emitByte(@intFromEnum(OpCode.ADD)),
            TokenType.MINUS => emitByte(@intFromEnum(OpCode.SUBTRACT)),
            TokenType.STAR => emitByte(@intFromEnum(OpCode.MULTIPLY)),
            TokenType.SLASH => emitByte(@intFromEnum(OpCode.DIVIDE)),
            else => return,
        }
    }
};

const ParseRule = struct {
    prefix: ?*const ParseFn,
    infix: ?*const ParseFn,
    precedence: Precedence,
};
const ParseFn = fn (*Parser) void;

fn getRule(tokenType: TokenType) ParseRule {
    return switch (tokenType) {
        .LEFT_PAREN => makeRule(Parser.grouping, null, .NONE),
        .RIGHT_PAREN => makeRule(null, null, .NONE),
        .LEFT_BRACE => makeRule(null, null, .NONE),
        .RIGHT_BRACE => makeRule(null, null, .NONE),
        .COMMA => makeRule(null, null, .NONE),
        .DOT => makeRule(null, null, .NONE),
        .MINUS => makeRule(Parser.unary, Parser.binary, .TERM),
        .PLUS => makeRule(null, Parser.binary, .TERM),
        .SEMICOLON => makeRule(null, null, .NONE),
        .SLASH => makeRule(null, Parser.binary, .FACTOR),
        .STAR => makeRule(null, Parser.binary, .FACTOR),
        .BANG => makeRule(null, null, .NONE),
        .BANG_EQUAL => makeRule(null, null, .NONE),
        .EQUAL => makeRule(null, null, .NONE),
        .EQUAL_EQUAL => makeRule(null, null, .NONE),
        .GREATER => makeRule(null, null, .NONE),
        .GREATER_EQUAL => makeRule(null, null, .NONE),
        .LESS => makeRule(null, null, .NONE),
        .LESS_EQUAL => makeRule(null, null, .NONE),
        .IDENTIFIER => makeRule(null, null, .NONE),
        .STRING => makeRule(null, null, .NONE),
        .NUMBER => makeRule(Parser.number, null, .NONE),
        .AND => makeRule(null, null, .NONE),
        .CLASS => makeRule(null, null, .NONE),
        .ELSE => makeRule(null, null, .NONE),
        .FALSE => makeRule(null, null, .NONE),
        .FOR => makeRule(null, null, .NONE),
        .FUN => makeRule(null, null, .NONE),
        .IF => makeRule(null, null, .NONE),
        .NIL => makeRule(null, null, .NONE),
        .OR => makeRule(null, null, .NONE),
        .PRINT => makeRule(null, null, .NONE),
        .RETURN => makeRule(null, null, .NONE),
        .SUPER => makeRule(null, null, .NONE),
        .THIS => makeRule(null, null, .NONE),
        .TRUE => makeRule(null, null, .NONE),
        .VAR => makeRule(null, null, .NONE),
        .WHILE => makeRule(null, null, .NONE),
        .ERROR => makeRule(null, null, .NONE),
        .EOF => makeRule(null, null, .NONE),
    };
}

fn makeRule(prefix: ?*const ParseFn, infix: ?*const ParseFn, precedence: Precedence) ParseRule {
    return ParseRule{
        .prefix = prefix,
        .infix = infix,
        .precedence = precedence,
    };
}
fn currentChunk() *Chunk {
    return compilingChunk;
}

pub fn compile(source: [:0]u8, chunk: *Chunk) bool {
    s.Scanner.init(source);
    compilingChunk = chunk;
    parser.advance();
    parser.expression();
    parser.consume(TokenType.EOF, "Expect end of expression.");
    endCompiler();
    return !parser.hadError;
}

fn endCompiler() void {
    emitReturn();

    if (DEBUG_PRINT_CODE) {
        if (!parser.hadError) {
            debug.disassembleChunk(currentChunk(), "code");
        }
    }
}

fn emitReturn() void {
    emitByte(@intFromEnum(OpCode.RETURN));
}

fn makeConstant(value: Value) u8 {
    const constant = currentChunk().addConstant(value);
    if (constant > std.math.maxInt(u8)) {
        parser.err("Too many constants in one chunk.");
        return 0;
    }

    return @truncate(constant);
}

fn emitConstant(value: Value) void {
    emitBytes(@intFromEnum(OpCode.CONSTANT), makeConstant(value));
}

fn emitBytes(byte1: u8, byte2: u8) void {
    emitByte(byte1);
    emitByte(byte2);
}

pub fn emitByte(byte: u8) void {
    currentChunk().write(&main.allocator, byte, parser.previous.line);
}
