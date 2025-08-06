const std = @import("std");
const VM = @import("../vm.zig");
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

pub fn userDataBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static UserData.test")) return testFn;

    return null;
}

var allocator: std.mem.Allocator = std.heap.c_allocator;
var data = [_]u8{ 'm', 'y', ' ', 'u', 's', 'e', 'r', ' ', 'd', 'a', 't', 'a' };
var otherData = [_]u8{ 'o', 't', 'h', 'e', 'r', ' ', 'u', 's', 'e', 'r', ' ', 'd', 'a', 't', 'a' };

fn testResizeFn(context: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ra: usize) bool {
    const gc = VM.ZrenVM.GcAllocator.cast(context);
    if (gc.vm.config.userData) |ud| if (@as([*]u8, @ptrCast(ud)) != data[0..].ptr) return false;

    if (new_len == 0) {
        gc.vm.rawAllocator.free(memory);
        return false;
    }

    return gc.vresize(gc.ptr, memory, alignment, new_len, ra);
}

fn testRemapFn(context: *anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ra: usize) ?[*]u8 {
    const gc = VM.ZrenVM.GcAllocator.cast(context);

    if (gc.vm.config.userData) |ud| if (@as([*]u8, @ptrCast(ud)) != data[0..].ptr) return null;

    if (new_len == 0) {
        gc.vm.rawAllocator.free(memory);
        return null;
    }

    return gc.vremap(gc.ptr, memory, alignment, new_len, ra);
}

fn testFn(vm: *VM.ZrenVM) void {
    var configuration = VM.ZrenConfiguration.init(vm.allocator);

    // 默认情况下应当是null.
    if (configuration.userData != null) {
        vm.setSlotBool(0, false);
        return;
    }

    const vtable: std.mem.Allocator.VTable = .{
        .alloc = allocator.vtable.alloc,
        .resize = testResizeFn,
        .remap = testRemapFn,
        .free = allocator.vtable.free,
    };

    const custom_allocator: std.mem.Allocator = .{
        .ptr = vm,
        .vtable = &vtable,
    };

    // 这里需要定义一个allocator, 将其realloc传入
    configuration.allocator = custom_allocator;
    configuration.userData = @ptrCast(@alignCast(data[0..].ptr));

    var otherVM = VM.ZrenVM.newVM(configuration);
    defer otherVM.deinit();
    // 应当能获取到.
    if (otherVM.getUserData()) |ud| if (@intFromPtr(ud) != @intFromPtr(data[0..].ptr)) {
        vm.setSlotBool(0, false);
        return;
    };

    // 应当能够设置.
    otherVM.setUserData(@ptrCast(otherData[0..].ptr));

    if (otherVM.getUserData()) |ud| if (@intFromPtr(ud) != @intFromPtr(otherData[0..].ptr)) {
        vm.setSlotBool(0, false);
        return;
    };

    vm.setSlotBool(0, true);
}
