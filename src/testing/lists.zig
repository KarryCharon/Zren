const std = @import("std");
const VM = @import("../vm.zig");

pub fn listsBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static Lists.newList()")) return newList;
    if (std.mem.eql(u8, signature, "static Lists.insert()")) return insert;
    if (std.mem.eql(u8, signature, "static Lists.set()")) return set;
    if (std.mem.eql(u8, signature, "static Lists.get(_,_)")) return get;

    return null;
}

fn newList(vm: *VM.ZrenVM) void {
    vm.setSlotNewList(0);
}

// 助手函数, 用于将一个双精度浮点数存储在一个槽中, 然后将其插入到位于零槽的列表中
fn insertNumber(vm: *VM.ZrenVM, index: isize, value: f64) void {
    vm.setSlotDouble(1, value);
    vm.insertInList(0, index, 1);
}

// 助手函数, 用于将一个双精度浮点数追加到槽中, 然后将其插入到位于零槽的列表中
fn appendNumber(vm: *VM.ZrenVM, value: f64) void {
    vm.setSlotDouble(1, value);
    vm.insertInList(0, -1, 1);
}

fn insert(vm: *VM.ZrenVM) void {
    vm.setSlotNewList(0);

    vm.ensureSlots(2);

    // 追加
    insertNumber(vm, 0, 1.0);
    insertNumber(vm, 1, 2.0);
    insertNumber(vm, 2, 3.0);

    // 插入
    insertNumber(vm, 0, 4.0);
    insertNumber(vm, 1, 5.0);
    insertNumber(vm, 2, 6.0);

    // 负索引
    insertNumber(vm, -1, 7.0);
    insertNumber(vm, -2, 8.0);
    insertNumber(vm, -3, 9.0);
}

fn get(vm: *VM.ZrenVM) void {
    const listSlot = 1;
    const index: isize = @intFromFloat(vm.getSlotDouble(2));

    vm.getListElement(listSlot, index, 0);
}

fn set(vm: *VM.ZrenVM) void {
    vm.setSlotNewList(0);

    vm.ensureSlots(2);

    appendNumber(vm, 1.0);
    appendNumber(vm, 2.0);
    appendNumber(vm, 3.0);
    appendNumber(vm, 4.0);

    // 设置 list[2] = 33
    vm.setSlotDouble(1, 33);
    vm.setListElement(0, 2, 1);

    // 设置 list[-1] = 44
    vm.setSlotDouble(1, 44);
    vm.setListElement(0, -1, 1);
}
