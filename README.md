# zig-lox
A Zig implementation of a bytecode VM for the educational Lox programming language as taught in the book [Crafting Interpreters](https://craftinginterpreters.com).

The first half of the book covers a purely interpreted approach to implementing Lox. I did that in Go [here](https://github.com/braheezy/gravlax).

```console
> echo 'print 1 + 1;' | zig-lox --eval
2
```

## Usage
You need Zig. The `.devcontainer` can be used to easily provide that.

Helpful commands:

```bash
# Build an executable: zig-out/bin/zig-lox
zig build --summary all
# Run all tests
zig build test --summary all
# Run a file
zig-out/bin/zig-lox test.lox
# Or run the repl
zig-out/bin/zig-lox
# Or eval an expression over stdin
echo '1 + 1' | zig-out/bin/zig-lox --eval
```
