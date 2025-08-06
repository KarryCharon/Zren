const std = @import("std");
const print = std.debug.print;
const builtin = @import("builtin");

const Path = @import("../path.zig").Path;
const VM = @import("../vm.zig");
const T = @import("test.zig");
const C = @import("../constants.zig").C;
const APITest = @import("api_test.zig");
const ZrenVM = VM.ZrenVM;
const ZrenConfiguration = VM.ZrenConfiguration;
const ZrenInterpretResult = VM.ZrenInterpretResult;

var vm: *ZrenVM = undefined;

fn initVM(allocator: std.mem.Allocator, isApiTest: bool) *ZrenVM {
    var config = ZrenConfiguration.init(allocator);
    config.resolveModuleFn = T.resolveModulePathEx;
    config.loadModuleFn = T.loadModuleEx;
    config.writeFn = T.writeEx;
    config.errorFn = T.errorEx;
    config.initialHeapSize = 1024 * 1024 * 100;

    if (isApiTest) {
        config.bindForeignClassFn = APITest.APITest_bindForeignClass;
        config.bindForeignMethodFn = APITest.APITest_bindForeignMethod;
    }

    return ZrenVM.newVM(config);
}

const APITest_Run = @import("api_test.zig").APITest_Run;

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

var testName: []const u8 = undefined;

pub fn testRun() !u8 {
    const allocator = std.heap.c_allocator;

    // const handled = handleArgs(allocator);
    // if (handled != 0) return handled;

    var argsIterator = try std.process.argsWithAllocator(allocator);
    defer argsIterator.deinit();
    _ = argsIterator.next(); // 跳过可执行程序名

    var exitCode: u8 = 0;

    var script: []const u8 = "";
    script = argsIterator.next() orelse script;
    const isApiTest = T.isModuleAnAPITest(script);

    vm = initVM(allocator, isApiTest);
    defer vm.deinit();

    const result = T.runFile(vm, script);

    if (isApiTest) exitCode = APITest_Run(vm, script);

    if (result == .RESULT_COMPILE_ERROR) return C.EX_DATAERR;
    if (result == .RESULT_RUNTIME_ERROR) return C.EX_SOFTWARE;

    return exitCode;
}
