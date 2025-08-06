const std = @import("std");
const VM = @import("../vm.zig");

pub fn newVMBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static VM.nullConfig()")) return nullConfig;
    if (std.mem.eql(u8, signature, "static VM.multipleInterpretCalls()")) return multipleInterpretCalls;

    return null;
}

fn nullConfig(vm: *VM.ZrenVM) void {
    var otherVM = VM.ZrenVM.newVM(null);

    // 应当可以执行代码.
    const result = otherVM.interpret("main", "1 + 2");
    vm.setSlotBool(0, result == .RESULT_SUCCESS);

    otherVM.freeVM();
}

fn multipleInterpretCalls(vm: *VM.ZrenVM) void {
    var otherVM = VM.ZrenVM.newVM(null);
    var result: VM.ZrenInterpretResult = undefined;

    var correct: bool = true;

    // 句柄应当在Wren代码的多次调用之间有效.
    const absMethod = otherVM.makeCallHandle("abs");

    result = otherVM.interpret("main", "import \"random\" for Random");
    correct = correct and (result == .RESULT_SUCCESS);

    for (0..5) |i| {
        // 在`interpret()`之前调用`ensureSlots()`避免在之后引入潜在问题.
        otherVM.ensureSlots(2);

        // 调用一个外部函数应当成功.
        result = otherVM.interpret("main", "Random.new(12345)");
        correct = correct and (result == .RESULT_SUCCESS);

        otherVM.ensureSlots(2);
        otherVM.setSlotDouble(0, @floatFromInt(-@as(isize, @intCast(i))));
        result = otherVM.callHandle(absMethod);
        correct = correct and (result == .RESULT_SUCCESS);

        const absValue = otherVM.getSlotDouble(0);
        correct = correct and (absValue == @as(f64, @floatFromInt(i)));
    }

    vm.setSlotBool(0, correct);

    otherVM.releaseHandle(absMethod);
    otherVM.freeVM();
}
