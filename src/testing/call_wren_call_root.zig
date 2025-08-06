const std = @import("std");
const VM = @import("../vm.zig");
const C = @import("../constants.zig").C;
pub fn callWrenCallRootRunTests(vm: *VM.ZrenVM) u8 {
    var exitCode: u8 = 0;
    vm.ensureSlots(1);
    vm.getVariable("./test/api/call_wren_call_root", "Test", 0);
    const testClass = vm.getSlotHandle(0);

    const run = vm.makeCallHandle("run()");

    vm.ensureSlots(1);
    vm.setSlotHandle(0, testClass);
    const result = vm.callHandle(run);
    if (result == .RESULT_RUNTIME_ERROR) {
        exitCode = C.EX_SOFTWARE;
    } else {
        std.debug.print("Missing runtime error.\n", .{});
    }

    vm.releaseHandle(testClass);
    vm.releaseHandle(run);
    return exitCode;
}
