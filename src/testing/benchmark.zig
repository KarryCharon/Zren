const std = @import("std");
const VM = @import("../vm.zig");
const Utils = @import("../utils.zig");

pub fn benchmarkBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static Benchmark.arguments(_,_,_,_)")) return arguments;
    if (std.mem.eql(u8, signature, "static Benchmark.call(_)")) return call;
    return null;
}

fn arguments(vm: *VM.ZrenVM) void {
    var result: f64 = 0;

    result += vm.getSlotDouble(1);
    result += vm.getSlotDouble(2);
    result += vm.getSlotDouble(3);
    result += vm.getSlotDouble(4);

    vm.setSlotDouble(0, result);
}

const testScript =
    \\class Test {
    \\  static method(a, b, c, d) { a + b + c + d }
    \\}
    \\
;

fn call(vm: *VM.ZrenVM) void {
    const iterations: u64 = @intFromFloat(vm.getSlotDouble(1));
    // 由于VM不是可重入的，我们不能在这个foreign方法中调用.
    // 相反，我们创建一个新的VM来运行调用测试.
    const config: VM.ZrenConfiguration = .{};
    var otherVM = VM.ZrenVM.newVm(config);
    defer otherVM.deinit();

    _ = otherVM.interpret("main", testScript);

    const method = otherVM.makeCallHandle("method(_,_,_,_)");

    otherVM.ensureSlots(1);
    otherVM.getVariable("main", "Test", 0);
    const testClass = otherVM.getSlotHandle(0);

    var timer = std.time.Timer.start() catch unreachable;

    var result: f64 = 0;
    for (0..iterations) |_| {
        otherVM.ensureSlots(5);
        otherVM.setSlotHandle(0, testClass);
        otherVM.setSlotDouble(1, 1.0);
        otherVM.setSlotDouble(2, 2.0);
        otherVM.setSlotDouble(3, 3.0);
        otherVM.setSlotDouble(4, 4.0);

        _ = otherVM.callHandle(method);

        result += otherVM.getSlotDouble(0);
    }

    const elapsed: f64 = @as(f64, @floatFromInt(timer.read())) * 1e-9;
    otherVM.releaseHandle(testClass);
    otherVM.releaseHandle(method);

    if (result == (1.0 + 2.0 + 3.0 + 4.0) * @as(f64, @floatFromInt(iterations))) {
        vm.setSlotDouble(0, elapsed);
    } else {
        vm.setSlotBool(0, false); // 错误结果
    }
}
