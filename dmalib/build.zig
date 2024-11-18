const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // lib
    const lib = b.addStaticLibrary(.{
        .name = "dmalib",
        .target = target,
        .optimize = optimize,
    });
    lib.linkLibC();
    lib.addCSourceFiles(.{
        .root = b.path("src/"),
        .files = &.{
            "dma.c",
            "log.c",
        },
        .flags = &.{
            "-O3",
            "-Wall",
            "-Wextra",
            "-std=c11",
        },
    });
    lib.installHeadersDirectory(b.path("./include"), "", .{
        .include_extensions = &.{
            "log.h",
            "pattern.h",
        },
    });
    b.installArtifact(lib);

    // test exe
    const dmalib = b.addExecutable(.{
        .name = "dmalib",
        .target = target,
        .optimize = optimize,
    });

    dmalib.addCSourceFiles(.{
        .root = b.path("tests/"),
        .files = &.{
            "test_pattern.c",
        },
        .flags = &.{
            "-O3",
            "-Wall",
            "-Wextra",
            "-std=c11",
        },
    });

    dmalib.addIncludePath(b.path("./include"));

    dmalib.linkLibrary(lib);

    dmalib.linkLibC();

    b.installArtifact(dmalib);
    b.installArtifact(dmalib);
}
