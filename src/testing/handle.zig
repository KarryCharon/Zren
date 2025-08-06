const std = @import("std");
const VM = @import("../vm.zig");

var handle: ?*VM.ZrenHandle = null;

pub fn handleBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static Handle.value=(_)")) return setValue;
    if (std.mem.eql(u8, signature, "static Handle.value")) return getValue;

    return null;
}

fn setValue(vm: *VM.ZrenVM) void {
    handle = vm.getSlotHandle(1);
}

fn getValue(vm: *VM.ZrenVM) void {
    vm.setSlotHandle(0, handle);
    vm.releaseHandle(handle);
}
