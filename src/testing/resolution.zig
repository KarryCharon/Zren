const std = @import("std");
const VM = @import("../vm.zig");

pub fn resolutionBindMethod(signature: []const u8) ?VM.ZrenForeignMethodFn {
    if (std.mem.eql(u8, signature, "static Resolution.noResolver()")) return noResolver;
    if (std.mem.eql(u8, signature, "static Resolution.returnsNull()")) return returnsNull;
    if (std.mem.eql(u8, signature, "static Resolution.changesString()")) return changesString;
    if (std.mem.eql(u8, signature, "static Resolution.shared()")) return shared;
    if (std.mem.eql(u8, signature, "static Resolution.importer()")) return importerFn;

    return null;
}

fn writeFn(vm: *VM.ZrenVM, text: []const u8) void {
    _ = vm;
    _ = std.io.getStdOut().writer().print("{s}", .{text}) catch unreachable;
}

fn reportError(vm: *VM.ZrenVM, etype: VM.ZrenErrorType, module: []const u8, line: ?usize, message: []const u8) void {
    _ = .{ vm, module, line };
    if (etype == .ERROR_RUNTIME) _ = std.io.getStdOut().writer().print("{s}\n", .{message}) catch unreachable;
}

fn loadModuleComplete(vm: *VM.ZrenVM, module: []const u8, result: VM.ZrenLoadModuleResult) void {
    _ = module;
    vm.raw_allocator.free(result.source);
    //   free((void*)result.source);
}

fn loadModule(vm: *VM.ZrenVM, module: []const u8) VM.ZrenLoadModuleResult {
    _ = std.io.getStdOut().writer().print("loading {s}\n", .{module}) catch unreachable;

    var source: []const u8 = "";
    if (std.mem.eql(u8, module, "main/baz/bang")) {
        source = "import \"foo|bar\"";
    } else {
        source = "System.print(\"ok\")";
    }

    const string = vm.raw_allocator.alloc(u8, source.len) catch unreachable;
    std.mem.copyForwards(u8, string, source);

    const result: VM.ZrenLoadModuleResult = .{
        .on_complete = loadModuleComplete,
        .source = string,
    };
    return result;
}

fn runTestVM(vm: *VM.ZrenVM, configuration: *VM.ZrenConfiguration, source: []const u8) void {
    configuration.writeFn = writeFn;
    configuration.errorFn = reportError;
    configuration.loadModuleFn = loadModule;

    var other_vm = VM.ZrenVM.newVM(configuration.*);
    defer other_vm.deinit();

    // 应当可以执行代码
    const result = other_vm.interpret("main", source);
    if (result != .RESULT_SUCCESS) {
        vm.setSlotString(0, "error");
    } else {
        vm.setSlotString(0, "success");
    }
}

fn noResolver(vm: *VM.ZrenVM) void {
    var configuration = VM.ZrenConfiguration.init(vm.allocator);

    // 默认情况下，没有解析函数
    if (configuration.resolveModuleFn != null) {
        vm.setSlotString(0, "Did not have null resolve function.");
        return;
    }

    runTestVM(vm, &configuration, "import \"foo/bar\"");
}

fn resolveToNull(vm: *VM.ZrenVM, importer: []const u8, name: []const u8, buf: []u8) usize {
    _ = .{ vm, importer, name, buf };
    return 0;
}

fn returnsNull(vm: *VM.ZrenVM) void {
    var configuration = VM.ZrenConfiguration.init(vm.allocator);

    configuration.resolveModuleFn = resolveToNull;
    runTestVM(vm, &configuration, "import \"foo/bar\"");
}

fn resolveChange(vm: *VM.ZrenVM, importer: []const u8, name: []const u8, buf: []u8) usize {
    _ = vm;
    // 拼接导入者和name
    const all = std.fmt.bufPrint(buf, "{s}/{s}", .{ importer, name }) catch unreachable;
    // 将 "|" 替换为 "/".
    for (0..all.len) |i| {
        if (buf[i] == '|') buf[i] = '/';
    }

    return all.len;
}

fn changesString(vm: *VM.ZrenVM) void {
    var configuration = VM.ZrenConfiguration.init(vm.allocator);

    configuration.resolveModuleFn = resolveChange;
    runTestVM(vm, &configuration, "import \"foo|bar\"");
}

fn shared(vm: *VM.ZrenVM) void {
    var configuration = VM.ZrenConfiguration.init(vm.allocator);

    configuration.resolveModuleFn = resolveChange;
    runTestVM(vm, &configuration, "import \"foo|bar\"\nimport \"foo/bar\"");
}

fn importerFn(vm: *VM.ZrenVM) void {
    var configuration = VM.ZrenConfiguration.init(vm.allocator);

    configuration.resolveModuleFn = resolveChange;
    runTestVM(vm, &configuration, "import \"baz|bang\"");
}
