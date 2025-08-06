const std = @import("std");
const VM = @import("../vm.zig");

var finalized: f64 = 0.0;

pub fn foreignClassBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static ForeignClass.finalized")) return apiFinalized;
    if (std.mem.eql(u8, signature, "Counter.increment(_)")) return counterIncrement;
    if (std.mem.eql(u8, signature, "Counter.value")) return counterValue;
    if (std.mem.eql(u8, signature, "Point.translate(_,_,_)")) return pointTranslate;
    if (std.mem.eql(u8, signature, "Point.toString")) return pointToString;

    return null;
}

pub fn foreignClassBindClass(className: []const u8, methods: *VM.ZrenForeignClassMethods) void {
    if (std.mem.eql(u8, className, "Counter")) {
        methods.allocate = counterAllocate;
        return;
    }

    if (std.mem.eql(u8, className, "Point")) {
        methods.allocate = pointAllocate;
        return;
    }

    if (std.mem.eql(u8, className, "Resource")) {
        methods.allocate = resourceAllocate;
        methods.finalize = resourceFinalize;
        return;
    }

    if (std.mem.eql(u8, className, "BadClass")) {
        methods.allocate = badClassAllocate;
        return;
    }
}

fn apiFinalized(vm: *VM.ZrenVM) void {
    vm.setSlotDouble(0, finalized);
}

fn counterIncrement(vm: *VM.ZrenVM) void {
    const value: *f64 = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    value.* += vm.getSlotDouble(1);
}

fn counterValue(vm: *VM.ZrenVM) void {
    const value: *f64 = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    vm.setSlotDouble(0, value.*);
}

fn pointTranslate(vm: *VM.ZrenVM) void {
    const coordinates: [*]f64 = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    coordinates[0] += vm.getSlotDouble(1);
    coordinates[1] += vm.getSlotDouble(2);
    coordinates[2] += vm.getSlotDouble(3);
}

fn pointToString(vm: *VM.ZrenVM) void {
    const coordinates: [*]f64 = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    var result = [_]u8{0} ** 100;
    const res = std.fmt.bufPrint(&result, "({d}, {d}, {d})", .{ coordinates[0], coordinates[1], coordinates[2] }) catch unreachable;
    vm.setSlotString(0, res);
}

fn counterAllocate(vm: *VM.ZrenVM) void {
    const value: *f64 = vm.setSlotNewForeign(0, 0, f64);
    value.* = 0;
}

fn pointAllocate(vm: *VM.ZrenVM) void {
    var coordinates = vm.setSlotNewForeign(0, 0, [3]f64);

    // 该函数会被两个构造函数调用，所以通过检查槽的数量来查看调用了哪个构造函数
    if (vm.getSlotCount() == 1) {
        coordinates[0] = 0.0;
        coordinates[1] = 0.0;
        coordinates[2] = 0.0;
    } else {
        coordinates[0] = vm.getSlotDouble(1);
        coordinates[1] = vm.getSlotDouble(2);
        coordinates[2] = vm.getSlotDouble(3);
    }
}
fn resourceAllocate(vm: *VM.ZrenVM) void {
    const value = vm.setSlotNewForeign(0, 0, i32);
    value.* = 123;
}

fn resourceFinalize(data: *VM.ZrenVM) void {
    // 确保我们得到正确的数据
    const value: *i32 = @ptrCast(@alignCast(data));
    if (value.* != 123) std.process.exit(1);
    finalized += 1;
}

fn badClassAllocate(vm: *VM.ZrenVM) void {
    vm.ensureSlots(1);
    vm.setSlotString(0, "Something went wrong");
    vm.abortFiber(0);
}
