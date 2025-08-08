const std = @import("std");
const VM = @import("../vm.zig");

pub fn resetStackAfterForeignConstructBindClass(class_name: []const u8, methods: *VM.ZrenForeignClassMethods) void {
    if (std.mem.eql(u8, class_name, "ResetStackForeign")) {
        methods.allocate = counterAllocate;
        return;
    }
}

pub fn resetStackAfterForeignConstructRunTests(vm: *VM.ZrenVM) u8 {
    vm.ensureSlots(1);
    vm.getVariable("./test/api/reset_stack_after_foreign_construct", "Test", 0);
    const test_class = vm.getSlotHandle(0);

    const call_construct = vm.makeCallHandle("callConstruct()");
    const after_construct = vm.makeCallHandle("afterConstruct(_,_)");

    vm.ensureSlots(1);
    vm.setSlotHandle(0, test_class);
    _ = vm.callHandle(call_construct);

    vm.ensureSlots(3);
    vm.setSlotHandle(0, test_class);
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 2.0);
    _ = vm.callHandle(after_construct);

    vm.releaseHandle(test_class);
    vm.releaseHandle(call_construct);
    vm.releaseHandle(after_construct);

    return 0;
}

fn counterAllocate(vm: *VM.ZrenVM) void {
    const counter = vm.setSlotNewForeign(0, 0, f64);
    counter.* = vm.getSlotDouble(1);
}
