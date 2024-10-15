const std = @import("std");

pub fn build(b: *std.Build) void {
    const lib = b.addStaticLibrary(.{
        .name = "z",
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
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
}
