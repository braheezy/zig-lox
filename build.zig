const std = @import("std");
pub fn build(b: *std.Build) !void {
    const exe = b.addExecutable(.{
        .name = "zig-lox",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
    });

    b.installArtifact(exe);

    const debug = b.option(bool, "debug", "enable debug output") orelse false;
    const stress = b.option(bool, "stress", "enable gc stress") orelse false;
    const gclog = b.option(bool, "gclog", "enable gc log") orelse false;
    const nan_boxing = b.option(bool, "nanbox", "enable nan boxing") orelse false;
    var options = b.addOptions();
    options.addOption(bool, "debug", debug);
    options.addOption(bool, "stress_gc", stress);
    options.addOption(bool, "log_gc", gclog);
    options.addOption(bool, "nan_boxing", nan_boxing);

    exe.root_module.addOptions("config", options);

    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run");
    run_step.dependOn(&run_exe.step);
    if (b.args) |args| {
        run_exe.addArgs(args);
    }

    const main_test_step = b.step("test", "Run all tests");
    try addTests(b, exe, main_test_step);

    const stress_test_step = b.step("test-gc", "Run gc test");
    try addTests(b, exe, stress_test_step);
    const stress_test_exe = b.addExecutable(.{
        .name = "zig-loxd",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
    });
    options = b.addOptions();
    options.addOption(bool, "debug", debug);
    options.addOption(bool, "log_gc", gclog);
    options.addOption(bool, "stress_gc", true);
    stress_test_exe.root_module.addOptions("config", options);

    const stress_exe = b.addRunArtifact(stress_test_exe);
    const test_file_path = b.path("test/closed-upvalues-with-closures.lox");
    stress_exe.addFileArg(test_file_path);

    // Add expected stdout check
    stress_exe.expectStdOutEqual("updated\n");

    // Add the test step to the main test step
    stress_test_step.dependOn(&stress_exe.step);
}

fn addTests(b: *std.Build, exe: *std.Build.Step.Compile, test_step: *std.Build.Step) !void {
    // Define the test case structure
    const TestCase = struct {
        input: []const u8,
        expected_output: []const u8,
        is_file_test: bool = false,
    };

    // Define the test cases
    const test_cases = &[_]TestCase{
        // REPL tests
        .{ .input = "print 1 + 1;\n", .expected_output = "2\n" },
        .{ .input = "print 2 * 3;\n", .expected_output = "6\n" },
        .{ .input = "print 5 - 2;\n", .expected_output = "3\n" },
        .{ .input = "print !(5 - 4 > 3 * 2 == !nil);\n", .expected_output = "true\n" },
        .{ .input = "print 1 + 2;\n", .expected_output = "3\n" },
        .{ .input = "if (true or false) print \"pass\";\n", .expected_output = "pass\n" },
        .{ .input = "if (true and false) print \"pass\";\n", .expected_output = "" },
        .{ .input = "if (true and true) print \"pass\";\n", .expected_output = "pass\n" },
        .{
            .input =
            \\var breakfast = "beignets";
            \\var beverage = "cafe au lait";
            \\breakfast = "beignets with " + beverage;
            \\print breakfast;
            ,
            .expected_output = "beignets with cafe au lait\n",
        },
        .{
            .input =
            \\var a = 3;
            \\while (a > 0) { print a; a = a - 1; }
            ,
            .expected_output = "3\n2\n1\n",
        },
        .{
            .input =
            \\var a = 3;
            \\if (a > 0) { print "success"; }
            ,
            .expected_output = "success\n",
        },
        .{
            .input =
            \\fun fib(n) { if (n < 2) return n; return fib(n - 2) + fib(n - 1); }
            \\print fib(10);
            ,
            .expected_output = "55\n",
        },
        .{ .input = "for (var i = 0; i < 3; i = i + 1) print i;\n", .expected_output = "0\n1\n2\n" },
        // File test
        .{ .input = "test/closed-upvalues-with-closures.lox", .expected_output = "updated\n", .is_file_test = true },
        .{ .input = "test/basic-classes-and-instances.lox", .expected_output = "3\n", .is_file_test = true },
        .{ .input = "test/methods.lox", .expected_output = "not a method\n", .is_file_test = true },
    };

    // Iterate over the test cases and create test steps
    for (test_cases) |test_case| {
        if (test_case.is_file_test) {
            const test_file_exe = b.addRunArtifact(exe);
            const test_file_path = b.path(test_case.input);
            test_file_exe.addFileArg(test_file_path);

            // Add expected stdout check
            test_file_exe.expectStdOutEqual(test_case.expected_output);

            // Add the test step to the main test step
            test_step.dependOn(&test_file_exe.step);
        } else {
            const test_repl_exe = b.addRunArtifact(exe);
            test_repl_exe.addArg("--eval");

            // Set stdin data
            test_repl_exe.setStdIn(std.Build.Step.Run.StdIn{ .bytes = test_case.input });

            // Add expected stdout check
            test_repl_exe.expectStdOutEqual(test_case.expected_output);

            // Add the test step to the main test step
            test_step.dependOn(&test_repl_exe.step);
        }
    }
}
