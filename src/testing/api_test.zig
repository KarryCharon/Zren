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

var testName: []const u8 = "";

pub fn APITest_bindForeignMethod(vm: *VM.ZrenVM, module: []const u8, className: []const u8, isStatic: bool, signature: []const u8) ?VM.ZrenForeignMethodFn {
    _ = vm;
    if (!std.mem.startsWith(u8, module, "./test/")) return null;
    var buf = [_]u8{0} ** 256;
    var fullName: []u8 = &buf;
    if (isStatic) {
        fullName = std.fmt.bufPrint(&buf, "static {s}.{s}", .{ className, signature }) catch unreachable;
    } else {
        fullName = std.fmt.bufPrint(&buf, "{s}.{s}", .{ className, signature }) catch unreachable;
    }

    var method: ?VM.ZrenForeignMethodFn = null;
    method = benchmarkBindMethod(fullName);
    if (method) |m| return m;

    method = callCallsForeignBindMethod(fullName);
    if (method) |m| return m;

    method = errorBindMethod(fullName);
    if (method) |m| return m;

    method = foreignClassBindMethod(fullName);
    if (method) |m| return m;

    method = getVariableBindMethod(fullName);
    if (method) |m| return m;

    method = handleBindMethod(fullName);
    if (method) |m| return m;

    method = listsBindMethod(fullName);
    if (method) |m| return m;

    method = mapsBindMethod(fullName);
    if (method) |m| return m;

    method = newVMBindMethod(fullName);
    if (method) |m| return m;

    method = resolutionBindMethod(fullName);
    if (method) |m| return m;

    method = slotsBindMethod(fullName);
    if (method) |m| return m;

    method = userDataBindMethod(fullName);
    if (method) |m| return m;

    IO.stderr.print("Unknown foreign method '{s}' for test '{s}'\n", .{ fullName, testName });

    std.process.exit(1);

    return null;
}

pub fn APITest_bindForeignClass(vm: *VM.ZrenVM, module: []const u8, className: []const u8) ?VM.ZrenForeignClassMethods {
    _ = vm;
    var methods = VM.ZrenForeignClassMethods{};
    if (!std.mem.startsWith(u8, module, "./test/api")) return null;

    foreignClassBindClass(className, &methods);
    if (methods.allocate != null) return methods;

    resetStackAfterForeignConstructBindClass(className, &methods);
    if (methods.allocate != null) return methods;

    slotsBindClass(className, &methods);
    if (methods.allocate != null) return methods;

    IO.stderr.print("Unknown foreign class '{s}' for test '{s}'\n", .{ className, testName });

    std.process.exit(1);

    return methods;
}

pub fn APITest_Run(vm: *VM.ZrenVM, name: []const u8) u8 {
    testName = name;
    if (Utils.strContains(name, "/call.wren")) return callRunTests(vm);
    if (Utils.strContains(name, "/call_calls_foreign.wren")) return callCallsForeignRunTests(vm);
    if (Utils.strContains(name, "/call_wren_call_root.wren")) return callWrenCallRootRunTests(vm);
    if (Utils.strContains(name, "/reset_stack_after_call_abort.wren")) return resetStackAfterCallAbortRunTests(vm);
    if (Utils.strContains(name, "/reset_stack_after_foreign_construct.wren")) return resetStackAfterForeignConstructRunTests(vm);

    return 0;
}
