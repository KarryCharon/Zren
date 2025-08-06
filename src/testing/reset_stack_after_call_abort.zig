const std = @import("std");
const VM = @import("../vm.zig");

pub fn resetStackAfterCallAbortRunTests(vm: *VM.ZrenVM) u8 {
    vm.ensureSlots(1);
    vm.getVariable("./test/api/reset_stack_after_call_abort", "Test", 0);
    const testClass = vm.getSlotHandle(0);

    const abortFiber = vm.makeCallHandle("abortFiber()");
    const afterAbort = vm.makeCallHandle("afterAbort(_,_)");

    vm.ensureSlots(1);
    vm.setSlotHandle(0, testClass);
    _ = vm.callHandle(abortFiber);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, testClass);
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 2.0);
    _ = vm.callHandle(afterAbort);

    vm.releaseHandle(testClass);
    vm.releaseHandle(abortFiber);
    vm.releaseHandle(afterAbort);
    return 0;
}
