const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("extism", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    const tests = b.addTestExe("Library Tests", "test.zig");
    tests.addPackagePath("extism", "src/main.zig");
    tests.linkLibC();
    tests.addIncludePath("/usr/local/include");
    tests.addLibraryPath("/usr/local/lib");
    tests.linkSystemLibrary("extism");
    tests.setBuildMode(mode);
    tests.install();

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&tests.run().step);

    var example = b.addExecutable("Example", "examples/basic.zig");
    example.addPackagePath("extism", "src/main.zig");
    example.linkLibC();
    example.addIncludePath("/usr/local/include");
    example.addLibraryPath("/usr/local/lib");
    example.linkSystemLibrary("extism");
    example.setBuildMode(mode);
    example.setOutputDir("example-out");
    example.install();

    const example_run_cmd = example.run();

    const example_run_step = b.step("run_example", "Build basic_example");
    example_run_step.dependOn(&example_run_cmd.step);
}
