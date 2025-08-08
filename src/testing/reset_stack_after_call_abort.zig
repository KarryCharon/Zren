const std = @import("std");
const VM = @import("../vm.zig");

pub fn resetStackAfterCallAbortRunTests(vm: *VM.ZrenVM) u8 {
    vm.ensureSlots(1);
    vm.getVariable("./test/api/reset_stack_after_call_abort", "Test", 0);
    const test_class = vm.getSlotHandle(0);

    const abort_fiber = vm.makeCallHandle("abortFiber()");
    const after_abort = vm.makeCallHandle("afterAbort(_,_)");

    vm.ensureSlots(1);
    vm.setSlotHandle(0, test_class);
    _ = vm.callHandle(abort_fiber);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, test_class);
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 2.0);
    _ = vm.callHandle(after_abort);

    vm.releaseHandle(test_class);
    vm.releaseHandle(abort_fiber);
    vm.releaseHandle(after_abort);
    return 0;
}
