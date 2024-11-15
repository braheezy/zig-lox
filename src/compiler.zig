const std = @import("std");

const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const main = @import("main.zig");
const memory = @import("memory.zig");
const obj = @import("object.zig");
const scan = @import("scan.zig");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
const DEBUG_PRINT_CODE = main.DEBUG_PRINT_CODE;
const Token = scan.Token;
const TokenType = scan.TokenType;
const print = std.debug.print;
pub var parser: Parser = Parser{
    .current = undefined,
    .previous = undefined,
    .had_error = false,
    .panic_mode = false,
};
pub var current_compiler: ?*Compiler = null;

pub const UINT8_COUNT = std.math.maxInt(u8) + 1;
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

const Local = struct {
    name: Token,
    depth: i32,
    is_captured: bool,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const FunctionType = enum(u8) {
    Function,
    Script,
};

const ParseFn = fn (*Parser, bool) void;

fn getRule(token_type: TokenType) ParseRule {
    // print("[getRule] token_type: {s}\n", .{@tagName(token_type)});
    return switch (token_type) {
        .LEFT_PAREN => makeRule(Parser.grouping, Parser.call, .CALL),
        .RIGHT_PAREN => makeRule(null, null, .NONE),
        .LEFT_BRACE => makeRule(null, null, .NONE),
        .RIGHT_BRACE => makeRule(null, null, .NONE),
        .COMMA => makeRule(null, null, .NONE),
        .DOT => makeRule(null, Parser.dot, .CALL),
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
        .AND => makeRule(null, Parser.@"and", .AND),
        .CLASS => makeRule(null, null, .NONE),
        .ELSE => makeRule(null, null, .NONE),
        .FALSE => makeRule(Parser.literal, null, .NONE),
        .FOR => makeRule(null, null, .NONE),
        .FUN => makeRule(null, null, .NONE),
        .IF => makeRule(null, null, .NONE),
        .NIL => makeRule(Parser.literal, null, .NONE),
        .OR => makeRule(null, Parser.@"or", .OR),
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
    had_error: bool,
    panic_mode: bool,

    fn advance(self: *Parser) void {
        self.previous = self.current;

        while (true) {
            self.current = scan.scanner.scanToken();
            if (self.current.token_type != TokenType.ERROR) break;

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
        if (self.panic_mode) return;
        self.panic_mode = true;
        // print("[line {d}] Error", .{token.line});

        if (token.token_type == TokenType.EOF) {
            print(" at end", .{});
        } else if (token.token_type == TokenType.ERROR) {
            // nothing
        } else {
            print(" at {s}", .{token.start});
        }

        print(": {s}\n", .{message});
        self.had_error = true;
    }

    fn consume(self: *Parser, token_type: TokenType, message: []const u8) void {
        if (self.current.token_type == token_type) {
            self.advance();
            return;
        }

        self.errorAtCurrent(message);
    }

    fn check(self: *Parser, token_type: TokenType) bool {
        return self.current.token_type == token_type;
    }

    fn match(self: *Parser, token_type: TokenType) bool {
        if (!self.check(token_type)) return false;
        self.advance();
        return true;
    }

    fn expression(self: *Parser) void {
        // print("[expression]\n", .{});
        self.parsePrecedence(Precedence.ASSIGNMENT);
    }

    fn block(self: *Parser) std.mem.Allocator.Error!void {
        while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
            try self.declaration();
        }

        self.consume(.RIGHT_BRACE, "Expect '}' after block.");
    }

    fn varDeclaration(self: *Parser) void {
        const global = self.parseVariable("Expect variable name.");

        if (self.match(.EQUAL)) {
            self.expression();
        } else {
            current_compiler.?.emitByte(@intFromEnum(OpCode.NIL));
        }

        self.consume(.SEMICOLON, "Expect ';' after variable declaration.");

        current_compiler.?.defineVariable(global);
    }

    fn expressionStatement(self: *Parser) void {
        // print("[expressionStatement]\n", .{});
        self.expression();
        // print("[expressionStatement 1]\n", .{});
        self.consume(.SEMICOLON, "Expect ';' after value.");
        current_compiler.?.emitByte(@intFromEnum(OpCode.POP));
    }

    fn ifStatement(self: *Parser) std.mem.Allocator.Error!void {
        self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after condition");

        const then_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
        current_compiler.?.emitByte(@intFromEnum(OpCode.POP));
        try self.statement();

        const else_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP));

        current_compiler.?.patchJump(then_jump);
        current_compiler.?.emitByte(@intFromEnum(OpCode.POP));

        if (self.match(.ELSE)) try self.statement();

        current_compiler.?.patchJump(else_jump);
    }

    fn printStatement(self: *Parser) void {
        self.expression();
        self.consume(.SEMICOLON, "Expect ';' after value.");
        current_compiler.?.emitByte(@intFromEnum(OpCode.PRINT));
    }

    fn returnStatement(self: *Parser) void {
        if (current_compiler.?.func_type == .Script) self.err("Can't return from top-level code.");

        if (self.match(.SEMICOLON)) {
            current_compiler.?.emitReturn();
        } else {
            self.expression();
            self.consume(.SEMICOLON, "Expect ';' after return value;");
            current_compiler.?.emitByte(@intFromEnum(OpCode.RETURN));
        }
    }

    fn forStatement(self: *Parser) std.mem.Allocator.Error!void {
        current_compiler.?.beginScope();
        self.consume(.LEFT_PAREN, "Expect '(' after 'for'.");
        if (self.match(.SEMICOLON)) {
            // no initializer
        } else if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            self.expressionStatement();
        }

        var loop_start = current_compiler.?.currentChunk().len;
        var exit_jump: ?u8 = null;
        if (!self.match(.SEMICOLON)) {
            self.expression();
            self.consume(.SEMICOLON, "Expect ';' after loop condition.");
            // jump out of the loop if condition is false
            exit_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
            current_compiler.?.emitByte(@intFromEnum(OpCode.POP)); // condition
        }

        if (!self.match(.RIGHT_PAREN)) {
            const body_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP));
            const increment_start = current_compiler.?.currentChunk().len;
            self.expression();
            current_compiler.?.emitByte(@intFromEnum(OpCode.POP));
            self.consume(.RIGHT_PAREN, "Expect ')' after for clauses.");

            current_compiler.?.emitLoop(loop_start);
            loop_start = increment_start;
            current_compiler.?.patchJump(body_jump);
        }

        try self.statement();
        current_compiler.?.emitLoop(loop_start);
        if (exit_jump) |_| {
            current_compiler.?.patchJump(exit_jump.?);
            current_compiler.?.emitByte(@intFromEnum(OpCode.POP));
        }
        current_compiler.?.endScope();
    }

    fn whileStatement(self: *Parser) std.mem.Allocator.Error!void {
        const loop_start = current_compiler.?.currentChunk().len;
        self.consume(.LEFT_PAREN, "Expect '(' after 'while'.");
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after condition.");

        const exit_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
        current_compiler.?.emitByte(@intFromEnum(OpCode.POP));
        try self.statement();
        current_compiler.?.emitLoop(loop_start);

        current_compiler.?.patchJump(exit_jump);
        current_compiler.?.emitByte(@intFromEnum(OpCode.POP));
    }

    fn funDeclaration(self: *Parser) std.mem.Allocator.Error!void {
        // print("[funDeclaration]\n", .{});
        const global = self.parseVariable("Expect function name.");
        current_compiler.?.markInitialized();
        try self.function(FunctionType.Function);
        current_compiler.?.defineVariable(global);
    }

    fn classDeclaration(self: *Parser) void {
        self.consume(.IDENTIFIER, "Expect class name.");
        const name_constant = identifierConstant(&self.previous);
        self.declareVariable();

        current_compiler.?.emitBytes(@intFromEnum(OpCode.CLASS), name_constant);
        current_compiler.?.defineVariable(name_constant);

        self.consume(.LEFT_BRACE, "Expect '{' before class body.");
        self.consume(.RIGHT_BRACE, "Expect '}' after class body.");
    }

    fn declaration(self: *Parser) std.mem.Allocator.Error!void {
        if (self.match(.CLASS)) {
            self.classDeclaration();
        } else if (self.match(.FUN)) {
            try self.funDeclaration();
        } else if (self.match(.VAR)) {
            self.varDeclaration();
        } else {
            try self.statement();
        }

        if (self.panic_mode) self.synchronize();
    }

    fn synchronize(self: *Parser) void {
        self.panic_mode = false;

        while (self.current.token_type != .EOF) {
            if (self.previous.token_type == .SEMICOLON) return;
            switch (self.current.token_type) {
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

    fn statement(self: *Parser) !void {
        // print("[statement]\n", .{});
        if (self.match(.PRINT)) {
            self.printStatement();
        } else if (self.match(.WHILE)) {
            try self.whileStatement();
        } else if (self.match(.FOR)) {
            try self.forStatement();
        } else if (self.match(.LEFT_BRACE)) {
            current_compiler.?.beginScope();
            try self.block();
            current_compiler.?.endScope();
        } else if (self.match(.IF)) {
            try self.ifStatement();
        } else if (self.match(.RETURN)) {
            self.returnStatement();
        } else {
            self.expressionStatement();
        }
    }

    fn number(self: *Parser, can_assign: bool) void {
        // print("[parser.number]\n", .{});
        _ = can_assign;
        const value = std.fmt.parseFloat(f64, self.previous.start) catch 0.0;
        current_compiler.?.emitConstant(Value.number(value));
    }

    fn unary(self: *Parser, can_assign: bool) void {
        _ = can_assign;
        const operator_type = self.previous.token_type;

        // compile the operand
        self.parsePrecedence(Precedence.UNARY);

        // emit the operator instruction
        switch (operator_type) {
            .BANG => current_compiler.?.emitByte(@intFromEnum(OpCode.NOT)),
            .MINUS => current_compiler.?.emitByte(@intFromEnum(OpCode.NEGATE)),
            else => return,
        }
    }

    fn grouping(self: *Parser, can_assign: bool) void {
        // print("[grouping]\n", .{});
        _ = can_assign;
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
    }

    fn call(self: *Parser, can_assign: bool) void {
        // print("[call]\n", .{});
        _ = can_assign;

        const arg_count = self.argumentList();
        current_compiler.?.emitBytes(@intFromEnum(OpCode.CALL), arg_count);
    }

    fn argumentList(self: *Parser) u8 {
        var arg_count: u8 = 0;
        if (!self.check(.RIGHT_PAREN)) {
            while (true) {
                self.expression();
                if (arg_count == 255) {
                    self.err("Can't have more than 255 arguments.");
                }
                arg_count += 1;

                if (!self.match(.COMMA)) break;
            }
        }
        self.consume(.RIGHT_PAREN, "Expect ')' after arguments.");
        return arg_count;
    }

    fn function(self: *Parser, function_type: FunctionType) std.mem.Allocator.Error!void {
        // print("[function] current_compiler.?.upvalues[0].is_local: {any}\n", .{current_compiler.?.upvalues[0].is_local});
        try Compiler.init(main.vm, function_type);
        current_compiler.?.beginScope();

        self.consume(.LEFT_PAREN, "Expect '(' after function name.");
        if (!self.check(.RIGHT_PAREN)) {
            while (true) {
                current_compiler.?.function.arity += 1;
                if (current_compiler.?.function.arity > 255) {
                    self.errorAtCurrent("Can't have more than 255 parameters.");
                }
                const constant = self.parseVariable("Expect parameter name.");
                current_compiler.?.defineVariable(constant);

                if (!self.match(.COMMA)) break;
            }
        }
        self.consume(.RIGHT_PAREN, "Expect ')' after parameters.");
        self.consume(.LEFT_BRACE, "Expect '{' before function body.");
        try self.block();

        // const func = current_compiler.?.function;
        //? The C code didn't have to do this.
        const upvalues = current_compiler.?.upvalues;

        const func = current_compiler.?.endCompiler();
        // print("[function] func.upvalue_count: {d}\n", .{func.upvalue_count});

        current_compiler.?.emitBytes(@intFromEnum(OpCode.CLOSURE), current_compiler.?.makeConstant(Value.object(&func.obj)));

        for (0..func.upvalue_count) |i| {
            // print("[function] upvalues[i].is_local: {any}\n", .{upvalues[i].is_local});
            const is_local: u8 = if (upvalues[i].is_local) 1 else 0;
            const index = upvalues[i].index;
            // print("[function] emitting is_local: {d} and index: {d}\n", .{ is_local, index });
            current_compiler.?.emitByte(is_local);
            current_compiler.?.emitByte(index);
        }
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) void {
        // print("[parsePrecedence]\n", .{});
        self.advance();
        var rule = getRule(self.previous.token_type);
        var can_assign = false;
        if (rule.prefix) |prefix_rule| {
            can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
            prefix_rule(self, can_assign);
        } else {
            self.err("Expect expression.");
            return;
        }

        while (@intFromEnum(precedence) <= @intFromEnum(getRule(self.current.token_type).precedence)) {
            self.advance();
            rule = getRule(self.previous.token_type);
            if (rule.infix) |infix_rule| {
                infix_rule(self, can_assign);
            }
        }
        // print("[parsePrecedence 1]\n", .{});

        if (can_assign and self.match(.EQUAL)) {
            self.err("Invalid assignment target.");
        }
    }

    fn identifierConstant(name: *Token) u8 {
        const constant = obj.copyString(main.vm, name.start) catch |e| {
            std.debug.print("Failed to copy string: {any}", .{e});
            std.process.exit(1);
        };
        return current_compiler.?.makeConstant(Value.object(&constant.obj));
    }

    fn addLocal(self: *Parser, name: Token) void {
        if (current_compiler.?.local_count >= UINT8_COUNT) {
            self.err("Too many local variable in function.");
            return;
        }
        var local = &current_compiler.?.locals[current_compiler.?.local_count];
        current_compiler.?.local_count += 1;

        local.name = name;
        local.depth = -1;
        local.is_captured = false;
    }

    fn declareVariable(self: *Parser) void {
        if (current_compiler.?.scope_depth == 0) return;

        const name = &self.previous;

        var i = current_compiler.?.local_count;
        while (i > 0) : (i -= 1) {
            const index = i - 1;
            const local = current_compiler.?.locals[index];
            if (local.depth != -1 and local.depth < current_compiler.?.scope_depth) break;

            if (identifiersEqual(name, &local.name)) self.err("Already a variable with this name in this scope.");
        }
        self.addLocal(name.*);
    }

    fn parseVariable(self: *Parser, error_message: []const u8) u8 {
        self.consume(.IDENTIFIER, error_message);

        self.declareVariable();
        if (current_compiler.?.scope_depth > 0) return 0;

        return identifierConstant(&self.previous);
    }

    fn @"or"(self: *Parser, can_assign: bool) void {
        _ = can_assign;
        const else_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));
        const end_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP));

        current_compiler.?.patchJump(else_jump);
        current_compiler.?.emitByte(@intFromEnum(OpCode.POP));

        self.parsePrecedence(.OR);
        current_compiler.?.patchJump(end_jump);
    }

    fn @"and"(self: *Parser, can_assign: bool) void {
        _ = can_assign;
        const end_jump = current_compiler.?.emitJump(@intFromEnum(OpCode.JUMP_IF_FALSE));

        current_compiler.?.emitByte(@intFromEnum(OpCode.POP));
        self.parsePrecedence(.AND);

        current_compiler.?.patchJump(end_jump);
    }

    fn dot(self: *Parser, can_assign: bool) void {
        self.consume(.IDENTIFIER, "Expect property name after '.'.");
        const name = identifierConstant(&self.previous);

        if (can_assign and self.match(.EQUAL)) {
            self.expression();
            current_compiler.?.emitBytes(@intFromEnum(OpCode.SET_PROPERTY), name);
        } else {
            current_compiler.?.emitBytes(@intFromEnum(OpCode.GET_PROPERTY), name);
        }
    }

    fn binary(self: *Parser, can_assign: bool) void {
        _ = can_assign;
        const operator_type = self.previous.token_type;
        const rule = getRule(operator_type);
        self.parsePrecedence(rule.precedence.next());

        switch (operator_type) {
            .BANG_EQUAL => current_compiler.?.emitBytes(@intFromEnum(OpCode.EQUAL), @intFromEnum(OpCode.NOT)),
            .EQUAL_EQUAL => current_compiler.?.emitByte(@intFromEnum(OpCode.EQUAL)),
            .GREATER => current_compiler.?.emitByte(@intFromEnum(OpCode.GREATER)),
            .GREATER_EQUAL => current_compiler.?.emitBytes(@intFromEnum(OpCode.LESS), @intFromEnum(OpCode.NOT)),
            .LESS => current_compiler.?.emitByte(@intFromEnum(OpCode.LESS)),
            .LESS_EQUAL => current_compiler.?.emitBytes(@intFromEnum(OpCode.GREATER), @intFromEnum(OpCode.NOT)),
            .PLUS => current_compiler.?.emitByte(@intFromEnum(OpCode.ADD)),
            .MINUS => current_compiler.?.emitByte(@intFromEnum(OpCode.SUBTRACT)),
            .STAR => current_compiler.?.emitByte(@intFromEnum(OpCode.MULTIPLY)),
            .SLASH => current_compiler.?.emitByte(@intFromEnum(OpCode.DIVIDE)),
            else => return,
        }
    }

    fn literal(self: *Parser, can_assign: bool) void {
        _ = can_assign;
        switch (self.previous.token_type) {
            .FALSE => current_compiler.?.emitByte(@intFromEnum(OpCode.FALSE)),
            .NIL => current_compiler.?.emitByte(@intFromEnum(OpCode.NIL)),
            .TRUE => current_compiler.?.emitByte(@intFromEnum(OpCode.TRUE)),
            else => unreachable,
        }
    }

    fn string(self: *Parser, can_assign: bool) void {
        _ = can_assign;
        const lexeme = self.previous.start;

        // Ensure the string has at least two characters (the quotes)
        if (lexeme.len < 2) {
            self.err("String literal too short.");
            return;
        }

        // Extract the string content without the surrounding quotes
        const string_content = lexeme[1 .. lexeme.len - 1];

        // Create an ObjString by copying the string content
        const obj_string = obj.copyString(current_compiler.?.vm_allocator.vm.?, string_content) catch |e| {
            std.debug.print("Failed to copy string: {any}", .{e});
            std.process.exit(1);
        };

        // Emit the constant value
        current_compiler.?.emitConstant(Value.object(&obj_string.obj));
    }

    fn namedVariable(self: *Parser, name: *Token, can_assign: bool) void {
        var arg = current_compiler.?.resolveLocal(name);
        var get_op: OpCode = undefined;
        var setvop: OpCode = undefined;
        var byte_arg: u8 = 0;

        if (arg) |local_index| {
            get_op = .GET_LOCAL;
            setvop = .SET_LOCAL;

            byte_arg = local_index;
        } else {
            arg = current_compiler.?.resolveUpvalue(name);
            if (arg) |upvalue_index| {
                // print("[namedVariable] upvalue_index: {d} for name: {s}\n", .{ upvalue_index, name.start });
                get_op = .GET_UPVALUE;
                setvop = .SET_UPVALUE;
                byte_arg = upvalue_index;
            } else {
                // print("[namedVariable] got null upvalue for name: {s}\n", .{name.start});
                byte_arg = identifierConstant(name);
                get_op = .GET_GLOBAL;
                setvop = .SET_GLOBAL;
            }
        }

        if (can_assign and self.match(.EQUAL)) {
            self.expression();
            current_compiler.?.emitBytes(@intFromEnum(setvop), byte_arg);
        } else {
            current_compiler.?.emitBytes(@intFromEnum(get_op), byte_arg);
        }
    }

    fn variable(self: *Parser, can_assign: bool) void {
        self.namedVariable(&self.previous, can_assign);
    }
};

pub const Compiler = struct {
    enclosing: ?*Compiler,
    locals: [UINT8_COUNT]Local,
    local_count: u8,
    upvalues: [UINT8_COUNT]Upvalue,
    scope_depth: i32,
    function: *obj.ObjFunction,
    func_type: FunctionType,
    vm_allocator: *memory.VMAllocator,

    pub fn init(vm: *VM, func_type: FunctionType) !void {
        // print("[compiler.init] func_type: {s}\n", .{@tagName(func_type)});
        const compiler: *Compiler = try main.vm.allocator.create(Compiler);

        compiler.enclosing = current_compiler;
        compiler.function = try obj.newFunction(vm);
        compiler.func_type = func_type;
        compiler.local_count = 0;
        compiler.scope_depth = 0;
        compiler.vm_allocator = vm.allocator;
        current_compiler = compiler;

        if (func_type != .Script) {
            current_compiler.?.function.name = try obj.copyString(vm, parser.previous.start);
            // print("[compiler.init] function name: {s}\n", .{current_compiler.?.function.name.?.chars});
        }
        // claim slot 0 for vm usage
        const local = &current_compiler.?.locals[0];
        current_compiler.?.local_count += 1;
        local.depth = 0;
        local.name.start = "";
    }

    pub fn compile(self: *Compiler, source: [:0]u8) !?*obj.ObjFunction {
        scan.Scanner.init(source);
        parser.advance();

        while (!parser.match(.EOF)) {
            try parser.declaration();
        }

        const function = self.endCompiler();
        return if (parser.had_error) null else function;
    }

    fn endCompiler(self: *Compiler) *obj.ObjFunction {
        self.emitReturn();

        const function = self.function;
        // print("[endCompiler] function.upvalue_count: {d}\n", .{function.upvalue_count});

        if (DEBUG_PRINT_CODE) {
            if (!parser.had_error) {
                if (function.name) |name| {
                    debug.disassembleChunk(self.currentChunk(), name.chars) catch |err| {
                        print("Failed to disassemble chunk: {any}", .{err});
                        std.process.exit(1);
                    };
                } else {
                    debug.disassembleChunk(self.currentChunk(), "<script>") catch |err| {
                        print("Failed to disassemble chunk: {any}", .{err});
                        std.process.exit(1);
                    };
                }
            }
        }
        current_compiler = self.enclosing;
        self.vm_allocator.destroy(self);
        return function;
    }

    pub fn currentChunk(self: *Compiler) *Chunk {
        return &self.function.chunk;
    }

    fn resolveLocal(self: *Compiler, name: *Token) ?u8 {
        var i = self.local_count;
        while (i > 0) : (i -= 1) {
            const index = i - 1;
            const local = self.locals[index];
            if (identifiersEqual(name, &local.name)) return index;
        }
        return null;
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) ?u8 {
        const upvalue_count = self.function.upvalue_count;

        for (0..upvalue_count) |i| {
            const upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) return @intCast(i);
        }

        if (upvalue_count == UINT8_COUNT) {
            parser.err("Too many closure variables in function.");
            return 0;
        }

        // print("[addUpvalue] self.upvalues.len: {d}\n", .{self.upvalues.len});
        // print("[addUpvalue] upvalue_count: {d}\n", .{upvalue_count});
        // print("[addUpvalue] is_local: {any}\n", .{is_local});
        self.upvalues[upvalue_count] = Upvalue{
            .is_local = is_local,
            .index = index,
        };
        // print("[addUpvalue] self.upvalues[upvalue_count].is_local: {any}\n", .{self.upvalues[upvalue_count].is_local});
        self.function.upvalue_count += 1;
        // print("[addUpvalue] self.function.upvalue_count; {d}\n", .{self.function.upvalue_count});
        return self.function.upvalue_count - 1;
    }

    fn resolveUpvalue(self: *Compiler, name: *Token) ?u8 {
        if (self.enclosing) |enclosing| {
            if (enclosing.resolveLocal(name)) |local_index| {
                // print("[resolveUpvalue] got local_index\n", .{});
                enclosing.locals[local_index].is_captured = true;
                return self.addUpvalue(local_index, true);
            } else if (enclosing.resolveUpvalue(name)) |upvalue_index| {
                // print("[resolveUpvalue] got upvalue_index\n", .{});
                return self.addUpvalue(upvalue_index, false);
            } else return null;
        } else return null;
    }

    fn markInitialized(self: *Compiler) void {
        if (self.scope_depth == 0) return;

        if (self.local_count > 0) {
            const count = self.local_count - 1;
            self.locals[count].depth = self.scope_depth;
        }
    }

    fn defineVariable(self: ?*Compiler, global: u8) void {
        if (self) |s| {
            if (s.scope_depth > 0) {
                s.markInitialized();
                return;
            }

            s.emitBytes(@intFromEnum(OpCode.DEFINE_GLOBAL), global);
        }
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) void {
        self.scope_depth -= 1;

        while (self.local_count > 0 and
            self.locals[self.local_count - 1].depth > self.scope_depth)
        {
            if (self.locals[self.local_count - 1].is_captured) {
                self.emitByte(@intFromEnum(OpCode.CLOSE_UPVALUE));
            } else {
                self.emitByte(@intFromEnum(OpCode.POP));
            }
            self.local_count -= 1;
        }
    }

    fn emitReturn(self: *Compiler) void {
        self.emitByte(@intFromEnum(OpCode.NIL));
        self.emitByte(@intFromEnum(OpCode.RETURN));
    }

    fn makeConstant(self: *Compiler, value: Value) u8 {
        // print("[makeConstant 1] made: {any}\n", .{self.function.chunk.constants});
        const constant = self.currentChunk().addConstant(self.vm_allocator.vm.?, value);
        if (constant > std.math.maxInt(u8)) {
            parser.err("Too many constants in one chunk.");
            return 0;
        }
        // print("[makeConstant 2] made: {any}\n", .{self.function.chunk.constants});

        return @truncate(constant);
    }

    fn emitConstant(self: *Compiler, value: Value) void {
        self.emitBytes(@intFromEnum(OpCode.CONSTANT), self.makeConstant(value));
    }

    fn patchJump(self: *Compiler, offset: u8) void {
        // -2 to adjust for the bytecode for the jump offset itself
        const jump: u16 = self.currentChunk().len - offset - 2;

        if (jump > std.math.maxInt(u16)) {
            parser.err("Too much code to jump over");
        }

        self.currentChunk().code[offset] = @truncate((jump >> 8) & 0xff);
        self.currentChunk().code[offset + 1] = @truncate(jump & 0xff);
    }

    fn emitJump(self: *Compiler, instruction: u8) u8 {
        self.emitByte(instruction);
        self.emitByte(0xff);
        self.emitByte(0xff);
        return self.currentChunk().len - 2;
    }

    fn emitLoop(self: *Compiler, loop_start: usize) void {
        self.emitByte(@intFromEnum(OpCode.LOOP));

        const offset = self.currentChunk().len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) parser.err("Loop body too large");

        self.emitByte(@truncate((offset >> 8) & 0xff));
        self.emitByte(@truncate(offset & 0xff));
    }

    fn emitBytes(self: *Compiler, byte1: u8, byte2: u8) void {
        self.emitByte(byte1);
        self.emitByte(byte2);
    }

    pub fn emitByte(self: *Compiler, byte: u8) void {
        self.currentChunk().write(self.vm_allocator, byte, parser.previous.line);
    }
};

pub fn markCompilerRoots() void {
    // print("[markCompilerRoots 1]\n", .{});
    var compiler = current_compiler;
    while (compiler) |c| {
        // print("[markCompilerRoots 2]\n", .{});
        c.vm_allocator.markObject(&c.function.obj);
        compiler = c.enclosing;
    }
}

fn identifiersEqual(a: *const Token, b: *const Token) bool {
    if (a.start.len != b.start.len) return false;
    return std.mem.eql(u8, a.start, b.start);
}
