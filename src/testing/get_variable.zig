const std = @import("std");
const VM = @import("../vm.zig");

pub fn getVariableBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static GetVariable.beforeDefined()")) return beforeDefined;
    if (std.mem.eql(u8, signature, "static GetVariable.afterDefined()")) return afterDefined;
    if (std.mem.eql(u8, signature, "static GetVariable.afterAssigned()")) return afterAssigned;
    if (std.mem.eql(u8, signature, "static GetVariable.otherSlot()")) return otherSlot;
    if (std.mem.eql(u8, signature, "static GetVariable.otherModule()")) return otherModule;

    if (std.mem.eql(u8, signature, "static Has.variable(_,_)")) return hasVariable;
    if (std.mem.eql(u8, signature, "static Has.module(_)")) return hasModule;

    return null;
}

fn beforeDefined(vm: *VM.ZrenVM) void {
    vm.getVariable("./test/api/get_variable", "A", 0);
}

fn afterDefined(vm: *VM.ZrenVM) void {
    vm.getVariable("./test/api/get_variable", "A", 0);
}

fn afterAssigned(vm: *VM.ZrenVM) void {
    vm.getVariable("./test/api/get_variable", "A", 0);
}

fn otherSlot(vm: *VM.ZrenVM) void {
    vm.ensureSlots(3);
    vm.getVariable("./test/api/get_variable", "B", 2);

    // 移动到返回位置
    const string = vm.getSlotString(2);
    vm.setSlotString(0, string);
}

fn otherModule(vm: *VM.ZrenVM) void {
    vm.getVariable("./test/api/get_variable_module", "Variable", 0);
}

fn hasVariable(vm: *VM.ZrenVM) void {
    const module = vm.getSlotString(1);
    const variable = vm.getSlotString(2);

    const result = vm.hasVariable(module, variable);
    vm.ensureSlots(1);
    vm.setSlotBool(0, result);
}

fn hasModule(vm: *VM.ZrenVM) void {
    const module = vm.getSlotString(1);

    const result = vm.hasModule(module);
    vm.ensureSlots(1);
    vm.setSlotBool(0, result);
}
