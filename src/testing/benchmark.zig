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
    var other_vm = VM.ZrenVM.newVM(config);
    defer other_vm.deinit();

    _ = other_vm.interpret("main", testScript);

    const method = other_vm.makeCallHandle("method(_,_,_,_)");

    other_vm.ensureSlots(1);
    other_vm.getVariable("main", "Test", 0);
    const test_class = other_vm.getSlotHandle(0);

    var timer = std.time.Timer.start() catch unreachable;

    var result: f64 = 0;
    for (0..iterations) |_| {
        other_vm.ensureSlots(5);
        other_vm.setSlotHandle(0, test_class);
        other_vm.setSlotDouble(1, 1.0);
        other_vm.setSlotDouble(2, 2.0);
        other_vm.setSlotDouble(3, 3.0);
        other_vm.setSlotDouble(4, 4.0);

        _ = other_vm.callHandle(method);

        result += other_vm.getSlotDouble(0);
    }

    const elapsed: f64 = @as(f64, @floatFromInt(timer.read())) * 1e-9;
    other_vm.releaseHandle(test_class);
    other_vm.releaseHandle(method);

    if (result == (1.0 + 2.0 + 3.0 + 4.0) * @as(f64, @floatFromInt(iterations))) {
        vm.setSlotDouble(0, elapsed);
    } else {
        vm.setSlotBool(0, false); // 错误结果
    }
}
