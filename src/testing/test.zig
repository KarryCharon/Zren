const std = @import("std");
const print = std.debug.print;

const Constants = @import("../constants.zig");
const Path = @import("../path.zig").Path;
const VM = @import("../vm.zig");
const ZrenVM = VM.ZrenVM;
const ZrenErrorType = VM.ZrenErrorType;
const ZrenInterpretResult = VM.ZrenInterpretResult;
const ZrenLoadModuleResult = VM.ZrenLoadModuleResult;

pub fn isModuleAnAPITest(mmm: []const u8) bool {
    // TODO 改成使用 eql
    if (std.mem.indexOf(u8, mmm, "test/api") != null) return true;
    if (std.mem.indexOf(u8, mmm, "test/benchmark") != null) return true;
    return false;
}

pub fn runFile(vm: *ZrenVM, path: []const u8) ZrenInterpretResult {
    var module = Path.init(vm.raw_allocator, path) catch unreachable;
    defer module.deinit();
    var abs_module = module.resolveAbs() catch unreachable;
    defer abs_module.deinit();
    if (!abs_module.exists()) {
        std.debug.print("Error opening file: {s}\n", .{path});
        std.process.exit(Constants.C.EX_NOINPUT);
        return;
    }
    const source = abs_module.readBytes(vm.raw_allocator) catch unreachable;
    defer vm.raw_allocator.free(source);
    module.resolveRelInner() catch unreachable;
    if (module.pathType() == .PATH_TYPE_SIMPLE) {
        module.clear();
        module.push(".");
        module.joinInner(path);
    }
    module.removeExtension();
    return vm.interpret(module.toString(), source);
}

pub fn handleArgs(allocator: std.mem.Allocator) u8 {
    var argsIterator = try std.process.argsWithAllocator(allocator);
    defer argsIterator.deinit();
    const count = argsIterator.inner.count;
    _ = argsIterator.next(); // 跳过可执行程序名

    if (count < 2) {
        print("This is a Zren test runner.\nUsage: zren_test [file]\n", .{});
        return 64;
    }

    const arg1 = argsIterator.next().?;

    if (count == 2 and std.mem.eql(u8, arg1, "--version")) {
        print("Zren_test is running on Zren version {s}\n", .{"0.4.0"});
        return 1;
    }
    return 0;
}

pub fn writeEx(self: *ZrenVM, msg: []const u8) void {
    _ = self;
    const i = std.mem.indexOf(u8, msg, "\x00") orelse msg.len;
    _ = std.io.getStdOut().write(msg[0..i]) catch {};
}

pub fn errorEx(self: *ZrenVM, error_type: ZrenErrorType, module: []const u8, line: ?usize, msg: []const u8) void {
    _ = self;
    const o = std.io.getStdErr().writer();
    switch (error_type) {
        .ERROR_COMPILE => o.print("[{s} line {d}] {s}\n", .{ module, line.?, msg }) catch {},
        .ERROR_STACK_TRACE => o.print("[{s} line {d}] in {s}\n", .{ module, line.?, msg }) catch {},
        .ERROR_RUNTIME => o.print("{s}\n", .{msg}) catch {},
    }
}

// 根据module加载源码
pub fn loadModuleEx(self: *ZrenVM, module: []const u8) ZrenLoadModuleResult {
    var path = Path.init(self.raw_allocator, module) catch unreachable;
    defer path.deinit();
    path.push(".wren");
    var result: ZrenLoadModuleResult = .{};
    result.source = path.readBytes(self.raw_allocator) catch &.{};
    return result;
}

// 解析模块绝对路径
pub fn resolveModulePathEx(self: *ZrenVM, importer: []const u8, module: []const u8, buf: []u8) usize {
    var path = Path.init(self.raw_allocator, module) catch return 0;
    defer path.deinit();
    if (path.pathType() == .PATH_TYPE_SIMPLE) {
        std.mem.copyForwards(u8, buf, module);
        return module.len;
    }
    path.clear();
    path.push(importer);
    var target = path.parent() catch return 0;
    target.joinInner(module);
    target.resolveRelInner() catch return 0;
    std.mem.copyForwards(u8, buf, target.toString());
    return target.toString().len;
}
