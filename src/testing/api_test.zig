const std = @import("std");
const VM = @import("../vm.zig");
const Utils = @import("../utils.zig");
const IO = @import("../io.zig");

const benchmarkBindMethod = @import("benchmark.zig").benchmarkBindMethod;
const callCallsForeignBindMethod = @import("call_calls_foreign.zig").callCallsForeignBindMethod;
const errorBindMethod = @import("error.zig").errorBindMethod;
const foreignClassBindMethod = @import("foreign_class.zig").foreignClassBindMethod;
const getVariableBindMethod = @import("get_variable.zig").getVariableBindMethod;
const handleBindMethod = @import("handle.zig").handleBindMethod;
const listsBindMethod = @import("lists.zig").listsBindMethod;
const mapsBindMethod = @import("maps.zig").mapsBindMethod;
const newVMBindMethod = @import("new_vm.zig").newVMBindMethod;
const resolutionBindMethod = @import("resolution.zig").resolutionBindMethod;
const slotsBindMethod = @import("slots.zig").slotsBindMethod;
const userDataBindMethod = @import("user_data.zig").userDataBindMethod;

const foreignClassBindClass = @import("foreign_class.zig").foreignClassBindClass;
const resetStackAfterForeignConstructBindClass = @import("reset_stack_after_foreign_construct.zig").resetStackAfterForeignConstructBindClass;
const slotsBindClass = @import("slots.zig").slotsBindClass;

const callRunTests = @import("call.zig").callRunTests;
const callCallsForeignRunTests = @import("call_calls_foreign.zig").callCallsForeignRunTests;
const callWrenCallRootRunTests = @import("call_wren_call_root.zig").callWrenCallRootRunTests;
const resetStackAfterCallAbortRunTests = @import("reset_stack_after_call_abort.zig").resetStackAfterCallAbortRunTests;
const resetStackAfterForeignConstructRunTests = @import("reset_stack_after_foreign_construct.zig").resetStackAfterForeignConstructRunTests;

var test_name: []const u8 = "";

pub fn APITest_bindForeignMethod(vm: *VM.ZrenVM, module: []const u8, class_name: []const u8, is_static: bool, signature: []const u8) ?VM.ZrenForeignMethodFn {
    _ = vm;
    if (!std.mem.startsWith(u8, module, "./test/")) return null;
    var buf = [_]u8{0} ** 256;
    var full_name: []u8 = &buf;
    if (is_static) {
        full_name = std.fmt.bufPrint(&buf, "static {s}.{s}", .{ class_name, signature }) catch unreachable;
    } else {
        full_name = std.fmt.bufPrint(&buf, "{s}.{s}", .{ class_name, signature }) catch unreachable;
    }

    var method: ?VM.ZrenForeignMethodFn = null;
    method = benchmarkBindMethod(full_name);
    if (method) |m| return m;

    method = callCallsForeignBindMethod(full_name);
    if (method) |m| return m;

    method = errorBindMethod(full_name);
    if (method) |m| return m;

    method = foreignClassBindMethod(full_name);
    if (method) |m| return m;

    method = getVariableBindMethod(full_name);
    if (method) |m| return m;

    method = handleBindMethod(full_name);
    if (method) |m| return m;

    method = listsBindMethod(full_name);
    if (method) |m| return m;

    method = mapsBindMethod(full_name);
    if (method) |m| return m;

    method = newVMBindMethod(full_name);
    if (method) |m| return m;

    method = resolutionBindMethod(full_name);
    if (method) |m| return m;

    method = slotsBindMethod(full_name);
    if (method) |m| return m;

    method = userDataBindMethod(full_name);
    if (method) |m| return m;

    IO.stderr.print("Unknown foreign method '{s}' for test '{s}'\n", .{ full_name, test_name });

    std.process.exit(1);

    return null;
}

pub fn APITest_bindForeignClass(vm: *VM.ZrenVM, module: []const u8, class_name: []const u8) ?VM.ZrenForeignClassMethods {
    _ = vm;
    var methods = VM.ZrenForeignClassMethods{};
    if (!std.mem.startsWith(u8, module, "./test/api")) return null;

    foreignClassBindClass(class_name, &methods);
    if (methods.allocate != null) return methods;

    resetStackAfterForeignConstructBindClass(class_name, &methods);
    if (methods.allocate != null) return methods;

    slotsBindClass(class_name, &methods);
    if (methods.allocate != null) return methods;

    IO.stderr.print("Unknown foreign class '{s}' for test '{s}'\n", .{ class_name, test_name });

    std.process.exit(1);

    return methods;
}

pub fn APITest_Run(vm: *VM.ZrenVM, name: []const u8) u8 {
    test_name = name;
    if (Utils.strContains(name, "/call.wren")) return callRunTests(vm);
    if (Utils.strContains(name, "/call_calls_foreign.wren")) return callCallsForeignRunTests(vm);
    if (Utils.strContains(name, "/call_wren_call_root.wren")) return callWrenCallRootRunTests(vm);
    if (Utils.strContains(name, "/reset_stack_after_call_abort.wren")) return resetStackAfterCallAbortRunTests(vm);
    if (Utils.strContains(name, "/reset_stack_after_foreign_construct.wren")) return resetStackAfterForeignConstructRunTests(vm);

    return 0;
}
