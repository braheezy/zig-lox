const std = @import("std");
pub fn build(b: *std.Build) !void {
    const exe = b.addExecutable(.{
        .name = "zig-lox",
        .root_source_file = b.path("src/main.zig"),
        .target = b.host,
    });

    b.installArtifact(exe);

    const debug = b.option(bool, "debug", "enable debug output") orelse false;
    const options = b.addOptions();
    options.addOption(bool, "debug", debug);

    exe.root_module.addOptions("config", options);

    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run");
    run_step.dependOn(&run_exe.step);
    if (b.args) |args| {
        run_exe.addArgs(args);
    }

    const main_test_step = b.step("test", "Run all tests");
    try addTests(b, exe, main_test_step);
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
        .{ .input = "1 + 1\n", .expected_output = "2\n" },
        .{ .input = "2 * 3\n", .expected_output = "6\n" },
        .{ .input = "5 - 2\n", .expected_output = "3\n" },
        .{ .input = "!(5 - 4 > 3 * 2 == !nil)\n", .expected_output = "true\n" },

        // File test
        .{ .input = "test.lox", .expected_output = "zig-lox\n", .is_file_test = true },
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
