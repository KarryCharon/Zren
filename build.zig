const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib_mod = b.createModule(.{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib = b.addLibrary(.{
        .name = "root_lib",
        .linkage = .static,
        .root_module = lib_mod,
    });

    b.installArtifact(lib);

    const exe = b.addExecutable(.{
        .name = "Zren",
        .root_module = exe_mod,
    });

    exe.linkLibC();

    exe.root_module.addImport("root_lib", lib_mod);

    // 添加自定义编译参数
    const nan_tagging = b.option(bool, "nan_tagging", "Enable nan tagging") orelse true;
    const options = b.addOptions();
    options.addOption(bool, "nan_tagging", nan_tagging);
    exe_mod.addOptions("build_options", options);

    b.installArtifact(exe);

    // 运行项
    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    run_cmd.step.dependOn(b.getInstallStep());
    const run = b.step("run", "Run the app");
    run.dependOn(&run_cmd.step);

    // 单元测试
    const run_test_cmd = b.addTest(.{ .root_module = lib_mod });
    const run_lib_unit_test = b.addRunArtifact(run_test_cmd);
    const test_step = b.step("test", "Run test");
    test_step.dependOn(&run_lib_unit_test.step);
}
