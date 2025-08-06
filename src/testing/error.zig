const std = @import("std");
const VM = @import("../vm.zig");

pub fn errorBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static Error.runtimeError")) return runtimeError;
    return null;
}

fn runtimeError(vm: *VM.ZrenVM) void {
    vm.ensureSlots(1);
    vm.setSlotString(0, "Error!");
    vm.abortFiber(0);
}
