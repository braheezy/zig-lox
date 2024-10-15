# zig-lox
> [!CAUTION]
> This project is in work! It is likely broken.

A Zig implementation of a bytecode VM for the educational Lox programming language as taught in the book [Crafting Interpreters](https://craftinginterpreters.com).

The first half of the book covers a purely interpreted approach to implementing Lox. I did that in Go [here](https://github.com/braheezy/gravlax).

## Usage
You need Zig. The `.devcontainer` can be used to easily provide that.

Helpful commands:

    # Build an executable: zig-out/bin/zig-lox
    zig build --summary all
    # Run all tests
    zig build test --summary all
