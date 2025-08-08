const std = @import("std");
const VM = @import("../vm.zig");

pub fn slotsBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static Slots.noSet")) return noSet;
    if (std.mem.eql(u8, signature, "static Slots.getSlots(_,_,_,_,_)")) return getSlots;
    if (std.mem.eql(u8, signature, "static Slots.setSlots(_,_,_,_,_)")) return setSlots;
    if (std.mem.eql(u8, signature, "static Slots.slotTypes(_,_,_,_,_,_,_,_)")) return slotTypes;
    if (std.mem.eql(u8, signature, "static Slots.ensure()")) return ensure;
    if (std.mem.eql(u8, signature, "static Slots.ensureOutsideForeign()")) return ensureOutsideForeign;
    if (std.mem.eql(u8, signature, "static Slots.getListCount(_)")) return getListCount;
    if (std.mem.eql(u8, signature, "static Slots.getListElement(_,_)")) return getListElement;
    if (std.mem.eql(u8, signature, "static Slots.getMapValue(_,_)")) return getMapValue;

    return null;
}

pub fn slotsBindClass(class_name: []const u8, methods: *VM.ZrenForeignClassMethods) void {
    _ = class_name;
    methods.allocate = foreignClassAllocate;
}

fn noSet(vm: *VM.ZrenVM) void {
    _ = vm; // 什么都不做.
}

fn getSlots(vm: *VM.ZrenVM) void {
    var result: bool = true;
    if (vm.getSlotBool(1) != true) result = false;

    const bytes = vm.getSlotBytes(2);
    if (bytes.len != 5) result = false;
    if (!std.mem.eql(u8, bytes[0..@min(5, bytes.len)], "by\x00te")) result = false;

    if (vm.getSlotDouble(3) != 1.5) result = false;
    if (!std.mem.eql(u8, vm.getSlotString(4), "str")) result = false;

    const handle = vm.getSlotHandle(5);

    if (result) {
        vm.setSlotHandle(0, handle); // 返回结果, 这样可以判定是否正确捕获.
    } else {
        vm.setSlotBool(0, false); // 如果遇到了任何错误, 返回false.
    }

    vm.releaseHandle(handle);
}

fn setSlots(vm: *VM.ZrenVM) void {
    const handle = vm.getSlotHandle(1);

    vm.setSlotBool(1, true);
    vm.setSlotBytes(2, "by\x00te");
    vm.setSlotDouble(3, 1.5);
    vm.setSlotString(4, "str");
    vm.setSlotNull(5);

    // 回读以确认是否正确设置.

    var result: bool = true;
    if (vm.getSlotBool(1) != true) result = false;

    const bytes = vm.getSlotBytes(2);
    if (bytes.len != 5) result = false;
    if (!std.mem.eql(u8, bytes[0..@min(5, bytes.len)], "by\x00te")) result = false;

    if (vm.getSlotDouble(3) != 1.5) result = false;
    if (!std.mem.eql(u8, vm.getSlotString(4), "str")) result = false;

    if (vm.getSlotType(5) != .TYPE_NULL) result = false;

    if (result) {
        vm.setSlotHandle(0, handle); // 移动到返回位置.
    } else {
        vm.setSlotBool(0, false); // 遇到错误, 返回false.
    }

    vm.releaseHandle(handle);
}

fn slotTypes(vm: *VM.ZrenVM) void {
    const result: bool =
        vm.getSlotType(1) == .TYPE_BOOL and
        vm.getSlotType(2) == .TYPE_FOREIGN and
        vm.getSlotType(3) == .TYPE_LIST and
        vm.getSlotType(4) == .TYPE_MAP and
        vm.getSlotType(5) == .TYPE_NULL and
        vm.getSlotType(6) == .TYPE_NUM and
        vm.getSlotType(7) == .TYPE_STRING and
        vm.getSlotType(8) == .TYPE_UNKNOWN;

    vm.setSlotBool(0, result);
}

fn ensure(vm: *VM.ZrenVM) void {
    const before = vm.getSlotCount();

    vm.ensureSlots(20);

    const after = vm.getSlotCount();

    // 使用插槽来确保它们可用
    for (0..20) |i| vm.setSlotDouble(i, @floatFromInt(i));

    var sum: i64 = 0;

    for (0..20) |i| sum += @intFromFloat(vm.getSlotDouble(i));

    var result = [_]u8{0} ** 100;
    const all = std.fmt.bufPrint(&result, "{d} -> {d} ({d})", .{ before, after, sum }) catch unreachable;

    vm.setSlotString(0, all);
}

fn ensureOutsideForeign(vm: *VM.ZrenVM) void {
    // 为了在foreign方法之外测试行为，创建一个新的单独的VM.
    const config = VM.ZrenConfiguration.init(vm.allocator);

    var other_vm = VM.ZrenVM.newVM(config);

    const before = other_vm.getSlotCount();

    other_vm.ensureSlots(20);

    const after = other_vm.getSlotCount();

    // 使用插槽来确保它们可用
    for (0..20) |i| other_vm.setSlotDouble(i, @floatFromInt(i));

    var sum: i64 = 0;

    for (0..20) |i| sum += @intFromFloat(other_vm.getSlotDouble(i));

    other_vm.freeVM();

    var result = [_]u8{0} ** 100;
    const all = std.fmt.bufPrint(&result, "{d} -> {d} ({d})", .{ before, after, sum }) catch unreachable;

    vm.setSlotString(0, all);
}

fn getListCount(vm: *VM.ZrenVM) void {
    vm.setSlotDouble(0, @floatFromInt(vm.getListCount(1)));
}

fn getListElement(vm: *VM.ZrenVM) void {
    const index: isize = @intFromFloat(vm.getSlotDouble(2));
    vm.getListElement(1, index, 0);
}

fn getMapValue(vm: *VM.ZrenVM) void {
    vm.getMapValue(1, 2, 0);
}

fn foreignClassAllocate(vm: *VM.ZrenVM) void {
    _ = vm.setSlotNewForeign(0, 0, u32);
}
