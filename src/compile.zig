const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const main = @import("main.zig");
const DEBUG_PRINT_CODE = main.DEBUG_PRINT_CODE;
const scan = @import("scan.zig");
const Token = scan.Token;
const TokenType = scan.TokenType;
const Value = @import("value.zig").Value;
const print = std.debug.print;
pub var parser: Parser = Parser{ .current = undefined, .previous = undefined, .hadError = false, .panicMode = false };
pub var global_compiler: Compiler = Compiler{ .compilingChunk = undefined };

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
        .BANG => makeRule(Parser.unary, null, .NONE),
        .BANG_EQUAL => makeRule(null, Parser.binary, .EQUALITY),
        .EQUAL => makeRule(null, null, .NONE),
        .EQUAL_EQUAL => makeRule(null, Parser.binary, .EQUALITY),
        .GREATER => makeRule(null, Parser.binary, .COMPARISON),
        .GREATER_EQUAL => makeRule(null, Parser.binary, .COMPARISON),
        .LESS => makeRule(null, Parser.binary, .COMPARISON),
        .LESS_EQUAL => makeRule(null, Parser.binary, .COMPARISON),
        .IDENTIFIER => makeRule(null, null, .NONE),
        .STRING => makeRule(null, null, .NONE),
        .NUMBER => makeRule(Parser.number, null, .NONE),
        .AND => makeRule(null, null, .NONE),
        .CLASS => makeRule(null, null, .NONE),
        .ELSE => makeRule(null, null, .NONE),
        .FALSE => makeRule(Parser.literal, null, .NONE),
        .FOR => makeRule(null, null, .NONE),
        .FUN => makeRule(null, null, .NONE),
        .IF => makeRule(null, null, .NONE),
        .NIL => makeRule(Parser.literal, null, .NONE),
        .OR => makeRule(null, null, .NONE),
        .PRINT => makeRule(null, null, .NONE),
        .RETURN => makeRule(null, null, .NONE),
        .SUPER => makeRule(null, null, .NONE),
        .THIS => makeRule(null, null, .NONE),
        .TRUE => makeRule(Parser.literal, null, .NONE),
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

const Parser = struct {
    current: Token,
    previous: Token,
    hadError: bool,
    panicMode: bool,

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = scan.scanner.scanToken();
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
        print("[line {d}] Error", .{token.line});

        if (token.tokenType == TokenType.EOF) {
            print(" at end", .{});
        } else if (token.tokenType == TokenType.ERROR) {
            // nothing
        } else {
            print(" at {s}", .{token.start});
        }

        print(": {s}\n", .{message});
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
        global_compiler.emitConstant(Value.number(value));
    }

    fn unary(self: *Parser) void {
        const operatorType = self.previous.tokenType;

        // compile the operand
        self.parsePrecedence(Precedence.UNARY);

        // emit the operator instruction
        switch (operatorType) {
            .BANG => global_compiler.emitByte(@intFromEnum(OpCode.NOT)),
            .MINUS => global_compiler.emitByte(@intFromEnum(OpCode.NEGATE)),
            else => return,
        }
    }

    fn grouping(self: *Parser) void {
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        var rule = getRule(self.previous.tokenType);
        // print("got rule: {any}, token type: {s}\n", .{ rule, @tagName(self.previous.tokenType) });
        if (rule.prefix) |prefixRule| {
            prefixRule(self);
        } else {
            self.err("Expect expression.");
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.tokenType).precedence)) {
            self.advance();
            rule = getRule(self.previous.tokenType);
            // print("got rule: {any}, token type: {s}\n", .{ rule, @tagName(self.previous.tokenType) });
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
            .BANG_EQUAL => global_compiler.emitBytes(@intFromEnum(OpCode.EQUAL), @intFromEnum(OpCode.NOT)),
            .EQUAL_EQUAL => global_compiler.emitByte(@intFromEnum(OpCode.EQUAL)),
            .GREATER => global_compiler.emitByte(@intFromEnum(OpCode.GREATER)),
            .GREATER_EQUAL => global_compiler.emitBytes(@intFromEnum(OpCode.LESS), @intFromEnum(OpCode.NOT)),
            .LESS => global_compiler.emitByte(@intFromEnum(OpCode.LESS)),
            .LESS_EQUAL => global_compiler.emitBytes(@intFromEnum(OpCode.GREATER), @intFromEnum(OpCode.NOT)),
            .PLUS => global_compiler.emitByte(@intFromEnum(OpCode.ADD)),
            .MINUS => global_compiler.emitByte(@intFromEnum(OpCode.SUBTRACT)),
            .STAR => global_compiler.emitByte(@intFromEnum(OpCode.MULTIPLY)),
            .SLASH => global_compiler.emitByte(@intFromEnum(OpCode.DIVIDE)),
            else => return,
        }
    }

    fn literal(self: *Parser) void {
        switch (self.previous.tokenType) {
            .FALSE => global_compiler.emitByte(@intFromEnum(OpCode.FALSE)),
            .NIL => global_compiler.emitByte(@intFromEnum(OpCode.NIL)),
            .TRUE => global_compiler.emitByte(@intFromEnum(OpCode.TRUE)),
            else => unreachable,
        }
    }
};

const Compiler = struct {
    compilingChunk: *Chunk,

    pub fn compile(self: *Compiler, source: [:0]u8, chunk: *Chunk) bool {
        scan.Scanner.init(source);
        self.compilingChunk = chunk;
        parser.advance();
        parser.expression();
        parser.consume(TokenType.EOF, "Expect end of expression.");
        self.endCompiler();
        return !parser.hadError;
    }

    fn endCompiler(self: *Compiler) void {
        self.emitReturn();

        if (DEBUG_PRINT_CODE) {
            if (!parser.hadError) {
                debug.disassembleChunk(self.compilingChunk, "code") catch {
                    parser.hadError = true;
                };
            }
        }
    }

    fn emitReturn(self: *Compiler) void {
        self.emitByte(@intFromEnum(OpCode.RETURN));
    }

    fn makeConstant(self: *Compiler, value: Value) u8 {
        const constant = self.compilingChunk.addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            parser.err("Too many constants in one chunk.");
            return 0;
        }

        return @truncate(constant);
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        self.emitBytes(@intFromEnum(OpCode.CONSTANT), self.makeConstant(value));
    }

    fn emitBytes(self: *Compiler, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    pub fn emitByte(self: *Compiler, byte: u8) void {
        self.compilingChunk.write(&main.allocator, byte, parser.previous.line);
    }
};
