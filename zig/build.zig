const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) void {
    comptime {
        const current_zig = builtin.zig_version;
        const min_zig = std.SemanticVersion.parse("0.11.0-dev.3834+d98147414") catch unreachable; // std.builtin.Version -> std.SemanticVersion
        if (current_zig.order(min_zig) == .lt) {
            @compileError(std.fmt.comptimePrint("Your Zig version v{} does not meet the minimum build requirement of v{}", .{ current_zig, min_zig }));
        }
    }

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "extism",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);

    var tests = b.addTest(.{
        .name = "Library Tests",
        .root_source_file = .{ .path = "test.zig" },
        .target = target,
        .optimize = optimize,
    });

    tests.addAnonymousModule("extism", .{ .source_file = .{ .path = "src/main.zig" } });
    tests.linkLibC();
    tests.addIncludePath(.{ .path = "/usr/local/include" });
    tests.addLibraryPath(.{ .path = "/usr/local/lib" });
    tests.linkSystemLibrary("extism");
    const tests_run_step = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&tests_run_step.step);

    var example = b.addExecutable(.{
        .name = "Example",
        .root_source_file = .{ .path = "examples/basic.zig" },
        .target = target,
        .optimize = optimize,
    });

    example.addAnonymousModule("extism", .{ .source_file = .{ .path = "src/main.zig" } });
    example.linkLibC();
    example.addIncludePath(.{ .path = "/usr/local/include" });
    example.addLibraryPath(.{ .path = "/usr/local/lib" });
    example.linkSystemLibrary("extism");
    const example_run_step = b.addRunArtifact(example);

    const example_step = b.step("run_example", "Build basic_example");
    example_step.dependOn(&example_run_step.step);
}
