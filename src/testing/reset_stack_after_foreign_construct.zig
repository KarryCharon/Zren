const std = @import("std");
const VM = @import("../vm.zig");

pub fn resetStackAfterForeignConstructBindClass(className: []const u8, methods: *VM.ZrenForeignClassMethods) void {
    if (std.mem.eql(u8, className, "ResetStackForeign")) {
        methods.allocate = counterAllocate;
        return;
    }
}

pub fn resetStackAfterForeignConstructRunTests(vm: *VM.ZrenVM) u8 {
    vm.ensureSlots(1);
    vm.getVariable("./test/api/reset_stack_after_foreign_construct", "Test", 0);
    const testClass = vm.getSlotHandle(0);

    const callConstruct = vm.makeCallHandle("callConstruct()");
    const afterConstruct = vm.makeCallHandle("afterConstruct(_,_)");

    vm.ensureSlots(1);
    vm.setSlotHandle(0, testClass);
    _ = vm.callHandle(callConstruct);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, testClass);
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 2.0);
    _ = vm.callHandle(afterConstruct);

    vm.releaseHandle(testClass);
    vm.releaseHandle(callConstruct);
    vm.releaseHandle(afterConstruct);

    return 0;
}

fn counterAllocate(vm: *VM.ZrenVM) void {
    const counter = vm.setSlotNewForeign(0, 0, f64);
    counter.* = vm.getSlotDouble(1);
}
