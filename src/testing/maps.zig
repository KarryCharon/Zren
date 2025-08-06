const std = @import("std");
const VM = @import("../vm.zig");

pub fn mapsBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static Maps.newMap()")) return newMap;
    if (std.mem.eql(u8, signature, "static Maps.insert()")) return insert;
    if (std.mem.eql(u8, signature, "static Maps.remove(_)")) return removeKey;
    if (std.mem.eql(u8, signature, "static Maps.count(_)")) return countWren;
    if (std.mem.eql(u8, signature, "static Maps.count()")) return countAPI;
    if (std.mem.eql(u8, signature, "static Maps.contains()")) return containsAPI;
    if (std.mem.eql(u8, signature, "static Maps.containsFalse()")) return containsAPIFalse;
    if (std.mem.eql(u8, signature, "static Maps.contains(_,_)")) return containsWren;
    if (std.mem.eql(u8, signature, "static Maps.invalidInsert(_)")) return invalidInsert;

    return null;
}

fn newMap(vm: *VM.ZrenVM) void {
    vm.setSlotNewMap(0);
}

fn invalidInsert(vm: *VM.ZrenVM) void {
    vm.setSlotNewMap(0);

    vm.ensureSlots(3);
    // 外部类位于槽位1
    vm.setSlotString(2, "England");
    vm.setMapValue(0, 1, 2); // 这里期望产生错误
}

fn insert(vm: *VM.ZrenVM) void {
    vm.setSlotNewMap(0);

    vm.ensureSlots(3);

    // 插入 String
    vm.setSlotString(1, "England");
    vm.setSlotString(2, "London");
    vm.setMapValue(0, 1, 2);

    // 插入 Double
    vm.setSlotDouble(1, 1.0);
    vm.setSlotDouble(2, 42.0);
    vm.setMapValue(0, 1, 2);

    // 插入 Boolean
    vm.setSlotBool(1, false);
    vm.setSlotBool(2, true);
    vm.setMapValue(0, 1, 2);

    // 插入 Null
    vm.setSlotNull(1);
    vm.setSlotNull(2);
    vm.setMapValue(0, 1, 2);

    // 插入 List
    vm.setSlotString(1, "Empty");
    vm.setSlotNewList(2);
    vm.setMapValue(0, 1, 2);
}

fn removeKey(vm: *VM.ZrenVM) void {
    vm.ensureSlots(3);

    vm.setSlotString(2, "key");
    vm.removeMapValue(1, 2, 0);
}

fn countWren(vm: *VM.ZrenVM) void {
    const count: f64 = @floatFromInt(vm.getMapCount(1));
    vm.setSlotDouble(0, count);
}

fn countAPI(vm: *VM.ZrenVM) void {
    insert(vm);
    const count: f64 = @floatFromInt(vm.getMapCount(0));
    vm.setSlotDouble(0, count);
}

fn containsWren(vm: *VM.ZrenVM) void {
    const result = vm.getMapContainsKey(1, 2);
    vm.setSlotBool(0, result);
}

fn containsAPI(vm: *VM.ZrenVM) void {
    insert(vm);

    vm.ensureSlots(1);
    vm.setSlotString(1, "England");

    const result = vm.getMapContainsKey(0, 1);
    vm.setSlotBool(0, result);
}

fn containsAPIFalse(vm: *VM.ZrenVM) void {
    insert(vm);

    vm.ensureSlots(1);
    vm.setSlotString(1, "DefinitelyNotARealKey");

    const result = vm.getMapContainsKey(0, 1);
    vm.setSlotBool(0, result);
}
