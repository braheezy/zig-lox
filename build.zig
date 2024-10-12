const std = @import("std");

pub fn build(b: *std.Build) void {
    const exe = b.addExecutable(.{
        .name = "zig-lox",
        .root_source_file = b.path("main.zig"),
        .target = b.host,
    });

    b.installArtifact(exe);

    const debug = b.option(bool, "debug", "enable debug output") orelse false;
    const options = b.addOptions();
    options.addOption(bool, "debug", debug);

    exe.root_module.addOptions("config", options);

    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);
}
