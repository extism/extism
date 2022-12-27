const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("extism-sdk", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    const main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);

    var example = b.addExecutable("Example", "examples/basic.zig");
    example.addPackagePath("extism-sdk", "src/main.zig");
    example.linkLibC();
    example.linkSystemLibrary("extism");
    example.setBuildMode(mode);
    example.setOutputDir("example-out");
    example.install();

    const example_run_cmd = example.run();
    example_run_cmd.step.dependOn(b.getInstallStep());

    const example_run_step = b.step("run_example", "Build basic_example");
    example_run_step.dependOn(&example_run_cmd.step);
}
