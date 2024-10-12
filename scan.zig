const std = @import("std");
pub const TokenType = enum(u8) {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    ERROR,
    EOF,
};

pub const Token = struct {
    tokenType: TokenType,
    start: []const u8,
    line: u32,
};

pub const Scanner = struct {
    source: [:0]const u8,
    start: usize,
    current: usize,
    line: u32,

    pub fn init(source: [:0]u8) Scanner {
        const scanner = Scanner{ .source = source, .start = 0, .current = 0, .line = 1 };
        return scanner;
    }

    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn isAlpha(c: u8) bool {
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isAtEnd()) return self.makeToken(TokenType.EOF);

        const c = self.advance();
        if (isAlpha(c)) return self.identifier();
        if (isDigit(c)) return self.number();
        switch (c) {
            '(' => return self.makeToken(TokenType.LEFT_PAREN),
            ')' => return self.makeToken(TokenType.RIGHT_PAREN),
            '{' => return self.makeToken(TokenType.LEFT_BRACE),
            '}' => return self.makeToken(TokenType.RIGHT_BRACE),
            ';' => return self.makeToken(TokenType.SEMICOLON),
            ',' => return self.makeToken(TokenType.COMMA),
            '.' => return self.makeToken(TokenType.DOT),
            '-' => return self.makeToken(TokenType.MINUS),
            '+' => return self.makeToken(TokenType.PLUS),
            '/' => return self.makeToken(TokenType.SLASH),
            '*' => return self.makeToken(TokenType.STAR),
            '!' => return self.makeToken(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG),
            '=' => return self.makeToken(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
            '<' => return self.makeToken(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS),
            '>' => return self.makeToken(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
            '"' => return self.string(),
            else => return self.errorToken("Unexpected character."),
        }
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        // comment goes until end of line
                        while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn checkKeyword(self: *Scanner, start: u8, length: u8, rest: []const u8, tokenType: TokenType) TokenType {
        if (((self.current - self.start) == start + length) and
            (std.mem.eql(u8, self.source[self.start + start .. self.start + start + length], rest)))
        {
            return tokenType;
        }

        return TokenType.IDENTIFIER;
    }

    fn identiferType(self: *Scanner) TokenType {
        switch (self.source[self.start]) {
            'a' => return self.checkKeyword(1, 2, "nd", TokenType.AND),
            'c' => return self.checkKeyword(1, 4, "lass", TokenType.CLASS),
            'e' => return self.checkKeyword(1, 3, "lse", TokenType.ELSE),
            'f' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'a' => return self.checkKeyword(2, 3, "lse", TokenType.FALSE),
                        'o' => return self.checkKeyword(2, 1, "r", TokenType.FOR),
                        'u' => return self.checkKeyword(2, 1, "n", TokenType.FUN),
                        else => {},
                    }
                }
            },
            'i' => return self.checkKeyword(1, 1, "f", TokenType.IF),
            'n' => return self.checkKeyword(1, 2, "il", TokenType.NIL),
            'o' => return self.checkKeyword(1, 1, "r", TokenType.OR),
            'p' => return self.checkKeyword(1, 4, "rint", TokenType.PRINT),
            'r' => return self.checkKeyword(1, 5, "eturn", TokenType.RETURN),
            's' => return self.checkKeyword(1, 4, "uper", TokenType.SUPER),
            't' => {
                if (self.current - self.start > 1) {
                    switch (self.source[self.start + 1]) {
                        'h' => return self.checkKeyword(2, 2, "is", TokenType.THIS),
                        't' => return self.checkKeyword(2, 2, "ue", TokenType.TRUE),
                        else => {},
                    }
                }
            },
            'v' => return self.checkKeyword(1, 2, "ar", TokenType.VAR),
            'w' => return self.checkKeyword(1, 4, "hile", TokenType.WHILE),
            else => {},
        }
        return TokenType.IDENTIFIER;
    }

    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) _ = self.advance();

        return self.makeToken(self.identiferType());
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) _ = self.advance();

        // look for a fractional part
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // consume the '.'
            _ = self.advance();

            while (isDigit(self.peek())) _ = self.advance();
        }

        return self.makeToken(TokenType.NUMBER);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.isAtEnd()) return self.errorToken("Unterminated string.");

        // the closing quote
        _ = self.advance();
        return self.makeToken(TokenType.STRING);
    }

    fn peek(self: *Scanner) u8 {
        return self.source[self.current];
    }

    fn peekNext(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current + 1];
    }

    fn makeToken(self: *Scanner, tokenType: TokenType) Token {
        return Token{
            .tokenType = tokenType,
            .start = self.source[self.start..self.current],
            .line = self.line,
        };
    }

    fn errorToken(self: *Scanner, message: []const u8) Token {
        return Token{
            .tokenType = TokenType.ERROR,
            .start = message,
            .line = self.line,
        };
    }
};
