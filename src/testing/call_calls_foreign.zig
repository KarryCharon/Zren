const std = @import("std");
const VM = @import("../vm.zig");
const IO = @import("../io.zig");

pub fn callCallsForeignBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static CallCallsForeign.api()")) return api;
    return null;
}

pub fn callCallsForeignRunTests(vm: *VM.ZrenVM) u8 {
    vm.ensureSlots(1);
    vm.getVariable("./test/api/call_calls_foreign", "CallCallsForeign", 0);
    const apiClass = vm.getSlotHandle(0);
    const call = vm.makeCallHandle("call(_)");

    vm.ensureSlots(2);
    vm.setSlotHandle(0, apiClass);
    vm.setSlotString(1, "parameter");

    IO.stdout.print("slots before {d}\n", .{vm.getSlotCount()});
    _ = vm.callHandle(call);

    // 这里应当仅有一个槽位.
    IO.stdout.print("slots after {d}\n", .{vm.getSlotCount()});

    vm.releaseHandle(call);
    vm.releaseHandle(apiClass);
    return 0;
}

fn api(vm: *VM.ZrenVM) void {
    // 增长slot数组. 这应当触发堆栈的移动.
    vm.ensureSlots(10);
    vm.setSlotNewList(0);

    for (1..10) |i| {
        vm.setSlotDouble(i, @floatFromInt(i));
        vm.insertInList(0, -1, i);
    }
}
