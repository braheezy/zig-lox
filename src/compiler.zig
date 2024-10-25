const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const main = @import("main.zig");
const obj = @import("object.zig");
const scan = @import("scan.zig");
const Value = @import("value.zig").Value;
const DEBUG_PRINT_CODE = main.DEBUG_PRINT_CODE;
const Token = scan.Token;
const TokenType = scan.TokenType;
const print = std.debug.print;
pub var parser: Parser = Parser{
    .current = undefined,
    .previous = undefined,
    .hadError = false,
    .panicMode = false,
};
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

const ParseFn = fn (*Parser, bool) void;

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
        .IDENTIFIER => makeRule(Parser.variable, null, .NONE),
        .STRING => makeRule(Parser.string, null, .NONE),
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

    fn check(self: *Parser, tokenType: TokenType) bool {
        return self.current.tokenType == tokenType;
    }

    fn match(self: *Parser, tokenType: TokenType) bool {
        if (!self.check(tokenType)) return false;
        self.advance();
        return true;
    }

    fn expression(self: *Parser) void {
        self.parsePrecedence(Precedence.ASSIGNMENT);
    }

    fn varDeclaration(self: *Parser) void {
        const global = self.parseVariable("Expect variable name.");

        if (self.match(.EQUAL)) {
            self.expression();
        } else {
            global_compiler.emitByte(@intFromEnum(OpCode.NIL));
        }

        self.consume(.SEMICOLON, "Expect ';' after variable declaration.");

        defineVariable(global);
    }

    fn expressionStatement(self: *Parser) void {
        self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        global_compiler.emitByte(@intFromEnum(OpCode.POP));
    }

    fn printStatement(self: *Parser) void {
        self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        global_compiler.emitByte(@intFromEnum(OpCode.PRINT));
    }

    fn declaration(self: *Parser) void {
        if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.statement();
        }

        if (self.panicMode) self.synchronize();
    }

    fn synchronize(self: *Parser) void {
        self.panicMode = false;

        while (self.current.tokenType != .EOF) {
            if (self.previous.tokenType == .SEMICOLON) return;
            switch (self.current.tokenType) {
                .CLASS => return,
                .FUN => return,
                .VAR => return,
                .FOR => return,
                .IF => return,
                .WHILE => return,
                .PRINT => return,
                .RETURN => return,
                else => continue,
            }
            self.advance();
        }
    }

    fn statement(self: *Parser) void {
        if (self.match(.PRINT)) {
            self.printStatement();
        } else {
            self.expressionStatement();
        }
    }

    fn number(self: *Parser, canAssign: bool) void {
        _ = canAssign;
        const value = std.fmt.parseFloat(f64, self.previous.start) catch 0.0;
        global_compiler.emitConstant(Value.number(value));
    }

    fn unary(self: *Parser, canAssign: bool) void {
        _ = canAssign;
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

    fn grouping(self: *Parser, canAssign: bool) void {
        _ = canAssign;
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        self.advance();
        var rule = getRule(self.previous.tokenType);
        var canAssign = false;
        // print("got rule: {any}, token type: {s}\n", .{ rule, @tagName(self.previous.tokenType) });
        if (rule.prefix) |prefixRule| {
            canAssign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
            prefixRule(self, canAssign);
        } else {
            self.err("Expect expression.");
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.tokenType).precedence)) {
            self.advance();
            rule = getRule(self.previous.tokenType);
            // print("got rule: {any}, token type: {s}\n", .{ rule, @tagName(self.previous.tokenType) });
            if (rule.infix) |infixRule| {
                infixRule(self, canAssign);
            }
        }

        if (canAssign and self.match(.EQUAL)) {
            self.err("Invalid assignment target.");
        }
    }

    fn identifierConstant(name: *Token) u8 {
        const constant = obj.copyString(name.start) catch |e| {
            std.debug.print("Failed to copy string: {any}", .{e});
            std.process.exit(1);
        };
        return global_compiler.makeConstant(Value.object(&constant.obj));
    }

    fn parseVariable(self: *Parser, errorMessage: []const u8) u8 {
        self.consume(.IDENTIFIER, errorMessage);
        return identifierConstant(&self.previous);
    }

    fn defineVariable(global: u8) void {
        global_compiler.emitBytes(@intFromEnum(OpCode.DEFINE_GLOBAL), global);
    }

    fn binary(self: *Parser, canAssign: bool) void {
        _ = canAssign;
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

    fn literal(self: *Parser, canAssign: bool) void {
        _ = canAssign;
        switch (self.previous.tokenType) {
            .FALSE => global_compiler.emitByte(@intFromEnum(OpCode.FALSE)),
            .NIL => global_compiler.emitByte(@intFromEnum(OpCode.NIL)),
            .TRUE => global_compiler.emitByte(@intFromEnum(OpCode.TRUE)),
            else => unreachable,
        }
    }

    fn string(self: *Parser, canAssign: bool) void {
        _ = canAssign;
        const lexeme = self.previous.start;

        // Ensure the string has at least two characters (the quotes)
        if (lexeme.len < 2) {
            self.err("String literal too short.");
            return;
        }

        // Extract the string content without the surrounding quotes
        const stringContent = lexeme[1 .. lexeme.len - 1];

        // Create an ObjString by copying the string content
        const objString = obj.copyString(stringContent) catch |e| {
            std.debug.print("Failed to copy string: {any}", .{e});
            std.process.exit(1);
        };

        // Emit the constant value
        global_compiler.emitConstant(Value.object(&objString.obj));
    }

    fn namedVariable(self: *Parser, name: *Token, canAssign: bool) void {
        const arg = identifierConstant(name);

        if (canAssign and self.match(.EQUAL)) {
            self.expression();
            global_compiler.emitBytes(@intFromEnum(OpCode.SET_GLOBAL), arg);
        } else {
            global_compiler.emitBytes(@intFromEnum(OpCode.GET_GLOBAL), arg);
        }
    }

    fn variable(self: *Parser, canAssign: bool) void {
        self.namedVariable(&self.previous, canAssign);
    }
};

const Compiler = struct {
    compilingChunk: *Chunk,

    pub fn compile(self: *Compiler, source: [:0]u8, chunk: *Chunk) bool {
        scan.Scanner.init(source);
        self.compilingChunk = chunk;
        parser.advance();

        while (!parser.match(.EOF)) {
            parser.declaration();
        }

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
