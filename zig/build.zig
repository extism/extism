const std = @import("std");

pub fn build(b: *std.Build) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "extism",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    lib.install();

    var tests = b.addTest(.{ .name = "Library Tests", .root_source_file = .{ .path = "test.zig" }, .target = target, .optimize = optimize, .kind = .test_exe });

    tests.addAnonymousModule("extism", .{ .source_file = .{ .path = "src/main.zig" } });
    tests.linkLibC();
    tests.addIncludePath("/usr/local/include");
    tests.addLibraryPath("/usr/local/lib");
    tests.linkSystemLibrary("extism");
    tests.install();

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&tests.run().step);

    var example = b.addExecutable(.{
        .name = "Example",
        .root_source_file = .{ .path = "examples/basic.zig" },
        .target = target,
        .optimize = optimize,
    });

    example.addAnonymousModule("extism", .{ .source_file = .{ .path = "src/main.zig" } });
    example.linkLibC();
    example.addIncludePath("/usr/local/include");
    example.addLibraryPath("/usr/local/lib");
    example.linkSystemLibrary("extism");
    example.setOutputDir("example-out");
    example.install();

    const example_run_cmd = example.run();

    const example_run_step = b.step("run_example", "Build basic_example");
    example_run_step.dependOn(&example_run_cmd.step);
}
