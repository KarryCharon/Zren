const std = @import("std");
const VM = @import("../vm.zig");
const IO = @import("../io.zig");

pub fn callRunTests(vm: *VM.ZrenVM) u8 {
    vm.ensureSlots(1);
    vm.getVariable("./test/api/call", "Call", 0);
    const callClass = vm.getSlotHandle(0);

    const noParams = vm.makeCallHandle("noParams");
    const zero = vm.makeCallHandle("zero()");
    const one = vm.makeCallHandle("one(_)");
    const two = vm.makeCallHandle("two(_,_)");
    const unary = vm.makeCallHandle("-");
    const binary = vm.makeCallHandle("-(_)");
    const subscript = vm.makeCallHandle("[_,_]");
    const subscriptSet = vm.makeCallHandle("[_,_]=(_)");

    // 不同参数数量
    vm.ensureSlots(1);
    vm.setSlotHandle(0, callClass);
    _ = vm.callHandle(noParams);

    vm.ensureSlots(1);
    vm.setSlotHandle(0, callClass);
    _ = vm.callHandle(zero);

    vm.ensureSlots(2);
    vm.setSlotHandle(0, callClass);
    vm.setSlotDouble(1, 1.0);
    _ = vm.callHandle(one);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, callClass);
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 2.0);
    _ = vm.callHandle(two);

    // 操作符
    vm.ensureSlots(1);
    vm.setSlotHandle(0, callClass);
    _ = vm.callHandle(unary);

    vm.ensureSlots(2);
    vm.setSlotHandle(0, callClass);
    vm.setSlotDouble(1, 1.0);
    _ = vm.callHandle(binary);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, callClass);
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 2.0);
    _ = vm.callHandle(subscript);

    vm.ensureSlots(4);
    vm.setSlotHandle(0, callClass);
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 2.0);
    vm.setSlotDouble(3, 3.0);
    _ = vm.callHandle(subscriptSet);

    // 返回一个value
    const getValue = vm.makeCallHandle("getValue()");
    vm.ensureSlots(1);
    vm.setSlotHandle(0, callClass);
    _ = vm.callHandle(getValue);
    IO.stdout.print("slots after call: {d}\n", .{vm.getSlotCount()});
    const value = vm.getSlotHandle(0);

    // 不同的参数类型
    vm.ensureSlots(3);
    vm.setSlotHandle(0, callClass);
    vm.setSlotBool(1, true);
    vm.setSlotBool(2, false);
    _ = vm.callHandle(two);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, callClass);
    vm.setSlotDouble(1, 1.2);
    vm.setSlotDouble(2, 3.4);
    _ = vm.callHandle(two);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, callClass);
    vm.setSlotString(1, "string");
    vm.setSlotString(2, "another");
    _ = vm.callHandle(two);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, callClass);
    vm.setSlotNull(1);
    vm.setSlotHandle(2, value);
    _ = vm.callHandle(two);

    // 截断字符串，或允许null字节
    vm.ensureSlots(3);
    vm.setSlotHandle(0, callClass);
    vm.setSlotBytes(1, "string"[0..3]);
    vm.setSlotBytes(2, "b\x00y\x00t\x00e"[0..7]);
    _ = vm.callHandle(two);

    // 调用时忽略栈上的临时槽
    vm.ensureSlots(10);
    vm.setSlotHandle(0, callClass);
    for (1..10) |i| vm.setSlotDouble(i, @as(f64, @floatFromInt(i)) * 0.1);

    _ = vm.callHandle(one);

    vm.releaseHandle(callClass);
    vm.releaseHandle(noParams);
    vm.releaseHandle(zero);
    vm.releaseHandle(one);
    vm.releaseHandle(two);
    vm.releaseHandle(getValue);
    vm.releaseHandle(value);
    vm.releaseHandle(unary);
    vm.releaseHandle(binary);
    vm.releaseHandle(subscript);
    vm.releaseHandle(subscriptSet);

    return 0;
}
