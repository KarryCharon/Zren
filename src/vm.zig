const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const Alignment = std.mem.Alignment;

const Constants = @import("constants.zig");
const Utils = @import("utils.zig");
const V = @import("value.zig");
const OpCode = @import("opcode.zig").OpCode;
const P = @import("parser.zig");
const Method = @import("method.zig").Method;
const Path = @import("path.zig").Path;

const Compiler = P.Compiler;
const Parser = P.Parser;

const ObjType = V.ObjType;
const Obj = V.Obj;
const ObjClass = V.ObjClass;
const ObjFiber = V.ObjFiber;
const ObjForeign = V.ObjForeign;
const ObjFunc = V.ObjFunc;
const ObjMap = V.ObjMap;
const ObjList = V.ObjList;
const ObjClosure = V.ObjClosure;
const ObjInstance = V.ObjInstance;
const ObjModule = V.ObjModule;
const ObjString = V.ObjString;
const ObjUpvalue = V.ObjUpvalue;
const ObjRange = V.ObjRange;

const FuncDebug = V.FuncDebug;

const CallFrame = V.CallFrame;

const SymbolTable = V.SymbolTable;
const ValueBuffer = V.ValueBuffer;
const UsizeBuffer = V.UsizeBuffer;
const ByteBuffer = V.ByteBuffer;
const MethodBuffer = V.MethodBuffer;

const Value = V.Value;

pub const ZrenHandle = struct {
    value: Value,
    prev: ?*ZrenHandle = null,
    next: ?*ZrenHandle = null,
};

pub const ZrenForeignMethodFn = *const fn (*ZrenVM) void;
pub const ZrenFinalizerFn = *const fn (*ZrenVM) void;

pub const ZrenForeignClassMethods = struct {
    allocate: ?ZrenForeignMethodFn = null,
    finalize: ?ZrenFinalizerFn = null,
};

pub const ZrenLoadModuleCompleteFn = *const fn (vm: *ZrenVM, name: []const u8, result: ZrenLoadModuleResult) void;

pub const ZrenLoadModuleResult = struct {
    source: []const u8 = &.{},
    onComplete: ?ZrenLoadModuleCompleteFn = undefined,
    userData: *anyopaque = undefined,
};

pub const ZrenInterpretResult = enum {
    RESULT_SUCCESS,
    RESULT_COMPILE_ERROR,
    RESULT_RUNTIME_ERROR,
};

pub const ZrenErrorType = enum {
    ERROR_COMPILE,
    ERROR_RUNTIME,
    ERROR_STACK_TRACE,
};

pub const ZrenType = enum {
    TYPE_BOOL,
    TYPE_NUM,
    TYPE_FOREIGN,
    TYPE_LIST,
    TYPE_MAP,
    TYPE_NULL,
    TYPE_STRING,

    TYPE_UNKNOWN, // 无法通过api访问的类型
};

pub const ZrenResolveModuleFn = *const fn (vm: *ZrenVM, importer: []const u8, name: []const u8, buf: []u8) usize;
pub const ZrenLoadModuleFn = *const fn (vm: *ZrenVM, name: []const u8) ZrenLoadModuleResult;
pub const ZrenBindForeignMethodFn = *const fn (vm: *ZrenVM, module: []const u8, className: []const u8, isStatic: bool, signature: []const u8) ?ZrenForeignMethodFn;
pub const ZrenBindForeignClassFn = *const fn (vm: *ZrenVM, module: []const u8, className: []const u8) ?ZrenForeignClassMethods;
pub const ZrenWriteFn = *const fn (vm: *ZrenVM, []const u8) void;
pub const ZrenErrorFn = *const fn (vm: *ZrenVM, errtype: ZrenErrorType, module: []const u8, line: ?usize, message: []const u8) void;

pub const ZrenConfiguration = struct {
    allocator: ?Allocator = null,
    resolveModuleFn: ?ZrenResolveModuleFn = null,
    loadModuleFn: ?ZrenLoadModuleFn = null,
    bindForeignClassFn: ?ZrenBindForeignClassFn = null,
    bindForeignMethodFn: ?ZrenBindForeignMethodFn = null,
    writeFn: ?ZrenWriteFn = null,
    errorFn: ?ZrenErrorFn = null,
    initialHeapSize: usize = 1024 * 1024 * 10,
    minHeapSize: usize = 1024 * 1024,
    heapGrowthPercent: usize = 50,
    userData: ?*anyopaque = null,

    pub const DEFAULT_CONFIG: @This() = .{ .allocator = std.heap.c_allocator };

    pub fn init(allocator: Allocator) ZrenConfiguration {
        return .{ .allocator = allocator };
    }
};

pub fn randomAllocate(vm: *ZrenVM) void {
    var well = vm.setSlotNewForeign(0, 0, Utils.Random);
    well.init();
}

pub fn randomSeed0(vm: *ZrenVM) void {
    var well: *Utils.Random = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    well.seed(0);
}

pub fn randomSeed1(vm: *ZrenVM) void {
    var well: *Utils.Random = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    well.seed(@intFromFloat(vm.getSlotDouble(1)));
}

pub fn randomSeed16(vm: *ZrenVM) void {
    var well: *Utils.Random = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    var comb: u64 = 0;
    for (0..16) |i| comb ^= @intFromFloat(vm.getSlotDouble(i + 1));
    well.seed(comb);
}

pub fn randomFloat(vm: *ZrenVM) void {
    var well: *Utils.Random = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    vm.setSlotDouble(0, well.float(f64));
}

pub fn randomInt0(vm: *ZrenVM) void {
    var well: *Utils.Random = @ptrCast(@alignCast(vm.getSlotForeign(0)));
    vm.setSlotDouble(0, @floatFromInt(well.int(i64)));
}

pub const ZrenVM = struct {
    // TODO 仍然需要改进, 比如 如何传递userData
    pub const GcAllocator = struct {
        allocator: Allocator,
        ptr: *anyopaque,
        valloc: *const fn (*anyopaque, len: usize, alignment: Alignment, ret_addr: usize) ?[*]u8,
        vresize: *const fn (*anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) bool,
        vremap: *const fn (*anyopaque, memory: []u8, alignment: Alignment, new_len: usize, ret_addr: usize) ?[*]u8,
        vfree: *const fn (*anyopaque, memory: []u8, alignment: Alignment, ret_addr: usize) void,
        vm: *ZrenVM,

        // 创建自定义分配器
        pub fn init(allocator: Allocator, vm: *ZrenVM) Allocator {
            const self: *@This() = allocator.create(@This()) catch @panic("OOM");
            vm.gcAllocatorRef = self; // 用于后续释放这里分配的内存
            self.allocator = allocator;
            self.ptr = allocator.ptr;
            self.valloc = allocator.vtable.alloc;
            self.vresize = allocator.vtable.resize;
            self.vremap = allocator.vtable.remap;
            self.vfree = allocator.vtable.free;
            self.vm = vm;
            return .{ .ptr = self, .vtable = &.{ .alloc = alloc, .resize = resize, .remap = remap, .free = free } };
        }

        pub inline fn cast(ctx: *anyopaque) *@This() {
            return @ptrCast(@alignCast(ctx));
        }

        // 内存分配函数
        pub fn alloc(context: *anyopaque, len: usize, alignment: Alignment, ra: usize) ?[*]u8 {
            // std.debug.print("分配内存: len={d}, alignment={d}, ra={x}\n", .{ len, alignment, ra });
            const self = cast(context);
            // self.vm.tryGarbageCollect(0, len); // TODO
            return self.valloc(self.ptr, len, alignment, ra);
        }

        // 内存调整函数
        pub fn resize(context: *anyopaque, memory: []u8, alignment: Alignment, newLen: usize, ra: usize) bool {
            const self = cast(context);
            // self.vm.tryGarbageCollect(memory.len, newLen); // TODO
            return self.vresize(self.ptr, memory, alignment, newLen, ra);
        }

        // 内存重映射函数
        pub fn remap(context: *anyopaque, memory: []u8, alignment: Alignment, newLen: usize, ra: usize) ?[*]u8 {
            const self = cast(context);
            // self.vm.tryGarbageCollect(memory.len, newLen); // TODO
            return self.vremap(self.ptr, memory, alignment, newLen, ra);
        }

        // 内存释放函数
        pub fn free(context: *anyopaque, memory: []u8, alignment: Alignment, ra: usize) void {
            const self = cast(context);
            return self.vfree(self.ptr, memory, alignment, ra);
        }
    };
    rawAllocator: Allocator = undefined,
    allocator: Allocator = undefined,
    gcAllocatorRef: ?*GcAllocator = null,

    boolClass: *ObjClass = undefined,
    classClass: *ObjClass = undefined,
    fiberClass: *ObjClass = undefined,
    fnClass: *ObjClass = undefined,
    listClass: *ObjClass = undefined,
    mapClass: *ObjClass = undefined,
    nullClass: *ObjClass = undefined,
    numClass: *ObjClass = undefined,
    objectClass: *ObjClass = undefined,
    rangeClass: *ObjClass = undefined,
    stringClass: *ObjClass = undefined,

    fiber: ?*ObjFiber = null,
    modules: *ObjMap = undefined,
    lastModule: ?*ObjModule = null,
    bytesAllocated: usize = 0,
    nextGC: usize = 0,

    first: ?*Obj = null,
    gray: std.ArrayList(*Obj) = undefined, // TODO 改进

    tempRoots: [Constants.C.MAX_TEMP_ROOTS]*Obj = undefined,
    numTempRoots: usize = 0,

    handles: ?*ZrenHandle = null,

    apiStack: []Value = &.{},
    apiStackOffset: usize = 0,

    config: ZrenConfiguration = undefined,
    compiler: ?*Compiler = null,
    methodNames: SymbolTable = undefined,

    fn readFile(self: *ZrenVM, path: []const u8) []const u8 {
        var file = std.fs.cwd().openFile(path, .{}) catch |err| {
            std.debug.print("Error opening file: {} -> {s}\n", .{ err, path });
            std.process.exit(Constants.C.EX_NOINPUT);
        };
        defer file.close();
        var meta = file.metadata() catch unreachable;
        return file.readToEndAlloc(self.rawAllocator, meta.size()) catch unreachable;
    }

    fn writeEx(self: *ZrenVM, msg: []const u8) void {
        _ = self;
        _ = std.io.getStdOut().write(msg) catch {};
    }

    fn errorEx(self: *ZrenVM, errorType: ZrenErrorType, module: []const u8, line: ?usize, msg: []const u8) void {
        _ = self;
        const o = std.io.getStdErr().writer();
        switch (errorType) {
            .ERROR_COMPILE => o.print("[{s} line {d}] {s}\n", .{ module, line.?, msg }) catch {},
            .ERROR_STACK_TRACE => o.print("[{s} line {d}] in {s}\n", .{ module, line.?, msg }) catch {},
            .ERROR_RUNTIME => o.print("{s}\n", .{msg}) catch {},
        }
    }

    // 根据module加载源码
    fn loadModuleEx(self: *ZrenVM, module: []const u8) ZrenLoadModuleResult {
        var path = Path.init(self.rawAllocator, module) catch unreachable;
        defer path.deinit();
        path.push(".wren");
        var result: ZrenLoadModuleResult = .{};
        result.source = path.readBytes(self.allocator) catch &.{};
        return result;
    }

    // 解析模块绝对路径
    fn resolveModulePathEx(self: *ZrenVM, importer: []const u8, module: []const u8, buf: []u8) usize {
        var path = Path.init(self.rawAllocator, module) catch return 0;
        defer path.deinit();
        if (path.pathType() == .PATH_TYPE_SIMPLE) {
            std.mem.copyForwards(u8, buf, module);
            return module.len;
        }
        path.clear();
        path.push(importer);
        var target = path.parent() catch return 0;
        target.joinInner(module);
        target.resolveInner() catch return 0;
        std.mem.copyForwards(u8, buf, target.toString());
        return target.toString().len;
    }

    pub fn init(alllocator: Allocator, isApiTest: bool) ZrenVM {
        var config = ZrenConfiguration.init(alllocator);
        config.resolveModuleFn = resolveModulePathEx;
        config.loadModuleFn = loadModuleEx;
        config.writeFn = writeEx;
        config.errorFn = errorEx;
        config.initialHeapSize = 1024 * 1024 * 100;
        // TODO
        if (isApiTest) {
            config.bindForeignClassFn = null;
            config.bindForeignMethodFn = null;
        }
        return newVM(config);
    }

    pub fn deinit(self: *@This()) void {
        Utils.assert(self.methodNames.count > 0, "VM appears to have already been freed.");
        var obj = self.first;
        while (obj) |o| {
            obj = o.next;
            self.freeObj(o);
        }

        self.gray.deinit();

        Utils.assert(self.handles == null, "All handles have not been released.");

        self.methodNames.deinit();

        self.allocator.destroy(self.gcAllocatorRef.?);
    }

    pub fn freeVM(self: *@This()) void {
        self.deinit();
    }

    pub fn tryGarbageCollect(self: *@This(), oldSize: usize, newSize: usize) void {
        self.bytesAllocated = self.bytesAllocated + newSize - oldSize;
        if (newSize > 0 and self.bytesAllocated > self.nextGC) {
            self.collectGarbage();
        }
    }

    pub fn collectGarbage(self: *@This()) void {
        comptime if (builtin.mode == .Debug) {
            // TODO 添加trace memory 和 trace gc
            // IO.stdout.print("------ gc ------\n", .{});
            // const before = self.bytesAllocated;
        };

        // 标记所有可到达的对象.

        // 重置此值. 当标记对象时, 它们的大小将被重新计算, 这样就可以跟踪使用多少内存, 而不需要知道每个*释放*对象的大小.
        // 这是非常重要的, 因为当释放一个未标记的对象时, 并不总是知道它使用多少内存.
        // 例如, 当释放一个实例时, 我们需要通过它的类来知道它的大小, 但它的类可能已经被释放了.
        self.bytesAllocated = 0;
        self.grayObj(self.modules.asObj());

        // 标记所有临时根对象
        for (0..self.numTempRoots) |i| self.grayObj(self.tempRoots[i]);

        self.grayObj(self.fiber.?.asObj()); // 标记当前fiber
        // 句柄
        var handle: ?*ZrenHandle = self.handles;
        while (handle) |h| : (handle = h.next) self.grayValue(h.value);

        self.markCompiler(self.compiler); // 编译器正在使用的任意对象(如果有的话).

        self.blackenSymbolTable(&self.methodNames); // 方法名

        self.blackenObjs(); // 深度优先搜索所有可到达的对象, 并标记为黑色

        // 收集标记为白色的对象.
        var obj = &self.first;
        while (obj.*) |o| {
            if (o.is_dark == false) {
                const unreached = o;
                obj.* = unreached.next;
                self.freeObj(unreached);
            } else {
                o.is_dark = false;
                obj = &o.next;
            }
        }
        // TODO 这里计算可能有问题 heapGrowthPercent
        self.nextGC = self.bytesAllocated + (self.bytesAllocated * self.config.heapGrowthPercent / 100);
        if (self.nextGC < self.config.minHeapSize) self.nextGC = self.config.minHeapSize;
        comptime if (builtin.mode == .Debug) {
            // TODO
            // #if WREN_DEBUG_TRACE_MEMORY || WREN_DEBUG_TRACE_GC
            //   double elapsed = ((double)clock() / CLOCKS_PER_SEC) - startTime;
            //   // 显式转换, 因为size_t在32位和64位上大小不同, 需要一种一致的类型来格式化字符串.
            //   printf("GC %lu before, %lu after (%lu collected), next at %lu. Took %.3fms.\n",
            //          (unsigned long)before,
            //          (unsigned long)vm->bytesAllocated,
            //          (unsigned long)(before - vm->bytesAllocated),
            //          (unsigned long)vm->nextGC,
            //          elapsed*1000.0);
            // #endif
        };
    }

    pub fn compileSource(self: *@This(), module: []const u8, source: []const u8, isExpression: bool, printErrors: bool) ?*ObjClosure {
        var nameValue: Value = .NULL_VAL;
        if (module.len > 0) {
            nameValue = self.newString(module);
            self.pushRoot(nameValue.asObj());
        }

        const closure = self.compileInModule(&nameValue, source, isExpression, printErrors);

        if (module.len > 0) self.popRoot();

        return closure;
    }

    pub fn compileInModule(self: *@This(), name: *Value, source: []const u8, isExpression: bool, printErrors: bool) ?*ObjClosure {
        var module = self.getModule(name.*);
        if (module == null) {
            module = self.newModule(name.asString());
            self.pushRoot(module.?.asObj());
            self.mapSet(self.modules, name.*, module.?.asObj().toVal());
            self.popRoot();
            var coreModule = self.getModule(.NULL_VAL);
            for (0..coreModule.?.variables.count) |i| {
                var v = coreModule.?.variables.at(i);
                const vn = coreModule.?.variableNames.at(i);
                _ = self.defineVariable(module.?, vn.value, &v, null);
            }
        }
        // TODO Should we still store the module even if it didn't compile?
        var func = self.compile(module.?, source, isExpression, printErrors) orelse return null;

        // 函数总被包装在闭包中.
        self.pushRoot(func.asObj());
        const closure = self.newClosure(func);
        self.popRoot();
        return closure;
    }

    pub fn compile(self: *@This(), module: *ObjModule, source: []const u8, isExpression: bool, printErrors: bool) ?*ObjFunc {
        const bomSize = Utils.bomSize(source);
        const csource = source[bomSize..];

        var parser: Parser = .{};
        parser.vm = self;
        parser.module = module;
        parser.source = csource;

        parser.tokenPos = 0;
        parser.charPos = 0;
        parser.currLine = 1;
        parser.numParens = 0;

        // 零初始化当前token. 该token将在后续调用 nextToken() 时被复制到 prev 中.
        parser.next.tokenType = .TOKEN_ERROR;
        parser.next.sourceRef = csource;
        parser.next.charStart = 0;
        parser.next.charNum = 0;
        parser.next.line = 0;
        parser.next.value = .UNDEFINED_VAL;

        parser.printErrors = printErrors;
        parser.hasError = false;

        parser.nextToken(); // 读取第一个token到 next
        parser.nextToken(); // 拷贝 next 到 curr

        const numExistingVariables = module.variables.count;

        var compiler = Compiler{};
        compiler.init(&parser, null, false);
        compiler.ignoreNewLines();

        if (isExpression) {
            compiler.expression();
            compiler.consume(.TOKEN_EOF, "Expect end of expression.");
        } else {
            while (!compiler.match(.TOKEN_EOF)) {
                compiler.definition();
                // 如果没有新的行, 则对应此行为文件末尾
                if (!compiler.matchLine()) {
                    compiler.consume(.TOKEN_EOF, "Expect end of file.");
                    break;
                }
            }
            compiler.emitOp(.CODE_END_MODULE);
        }
        compiler.emitOp(.CODE_RETURN);

        // 检查是否有隐式声明的模块级变量从未被显式定义.
        // 它们将具有表示变量首次被使用的行的数字值.
        for (numExistingVariables..parser.module.variables.count) |i| {
            var v = parser.module.variables.at(i);
            if (!v.isNum()) continue;
            parser.prev.tokenType = .TOKEN_NAME;
            parser.prev.sourceRef = parser.module.variableNames.at(i).value;
            parser.prev.charStart = 0;
            parser.prev.charNum = parser.module.variableNames.at(i).value.len;
            parser.prev.line = @intFromFloat(v.asNum());
            compiler.doError("Variable is used but not defined.", .{});
        }

        return compiler.endCompiler("(script)");
    }

    fn markCompiler(self: *@This(), inCompiler: ?*Compiler) void {
        const compiler: *Compiler = inCompiler orelse return;
        self.grayValue(compiler.parser.curr.value);
        self.grayValue(compiler.parser.prev.value);
        self.grayValue(compiler.parser.next.value);
        var _c: ?*Compiler = compiler;
        while (_c) |c| : (_c = c.parent) {
            if (c.func) |cf| self.grayObj(cf.asObj());
            if (c.constants) |cc| self.grayObj(cc.asObj());
            self.grayObj(c.attributes.?.asObj());
            if (c.enclosingClass) |ce| {
                self.blackenSymbolTable(&ce.fields);
                if (ce.methodAttributes) |ma| self.grayObj(ma.asObj());
                if (ce.classAttributes) |ca| self.grayObj(ca.asObj());
            }
        }
    }

    pub fn findVariable(self: *@This(), module: *ObjModule, name: []const u8) ?*Value {
        const symbol = self.symbolTableFind(&module.variableNames, name) orelse @panic("Variable not found");
        return module.variables.rat(symbol);
    }

    pub fn declareVariable(self: *@This(), module: *ObjModule, name: []const u8, line: usize) ?usize {
        // TODO 需要改进
        if (module.variables.count >= Constants.C.MAX_MODULE_VARS) return null;

        // 隐式定义的变量获得一个"value", 即变量首次被使用的行.
        // 我们将在稍后使用该值来报告正确的行号错误.
        module.variables.push(Value.numToValue(@floatFromInt(line)));
        return self.symbolTableAdd(&module.variableNames, name);
    }

    pub fn defineVariable(self: *@This(), module: *ObjModule, name: []const u8, value: *const Value, line: ?*i32) i32 {
        // TODO 应当改进返回值(比如使用tagged union)
        if (module.variables.count > Constants.C.MAX_MODULE_VARS) return -2;
        if (value.isObj()) self.pushRoot(value.asObj());

        var symbol: i32 = -1;
        if (self.symbolTableFind(&module.variableNames, name)) |i| {
            symbol = @intCast(i);
            var refv: *Value = module.variables.rat(i);
            // 隐式声明时，变量的值总是数字
            if (refv.isNum()) {
                // 现在我们有一个真正的定义
                if (line) |l| l.* = @intFromFloat(refv.asNum());
                refv.* = value.*;
                // 如果这是一个局部变量名, 则期望它在定义之前被引用时出错
                if (Utils.isLocalName(name[0])) symbol = -3;
            } else {
                // 已经显式定义
                symbol = -1;
            }
        } else {
            // 没找到，新建
            const str = self.newString(name).asString();
            self.pushRoot(&str.obj);
            module.variableNames.push(str);
            symbol = @intCast(module.variableNames.count - 1);
            self.popRoot();
            module.variables.push(value.*);
        }

        if (value.isObj()) self.popRoot();

        return symbol;
    }

    pub fn ensureSlots(vm: *@This(), numSlots: usize) void {
        // 如果没有可用的fiber，创建一个供API使用
        if (vm.isApiStackNull()) {
            vm.fiber = vm.newFiber(null);
            vm.setApiStack(vm.fiber.?.stack.buffer, 0);
        }

        const currentSize = vm.fiber.?.stack.top - vm.apiStackOffset;
        if (currentSize >= numSlots) return;

        // 增长栈
        const needed = vm.apiStackOffset + numSlots;
        vm.ensureStack(vm.fiber.?, needed);

        vm.fiber.?.loadStack(needed);
    }

    pub fn getSlotCount(self: *const @This()) usize {
        if (self.isApiStackNull()) return 0;
        return self.fiber.?.stack.top - self.apiStackOffset;
    }

    // 确保[slot]是API的槽位堆栈中的有效索引.
    inline fn validateApiSlot(vm: *@This(), slot: usize) void {
        Utils.assert(slot >= 0, "Slot cannot be negative.");
        Utils.assert(slot < vm.getSlotCount(), "Not that many slots.");
    }

    // 获取[slot]中对象的类型.
    pub fn getSlotType(vm: *@This(), slot: usize) ZrenType {
        vm.validateApiSlot(slot);
        if (vm.apiStackAt(slot).isBool()) return .TYPE_BOOL;
        if (vm.apiStackAt(slot).isNum()) return .TYPE_NUM;
        if (vm.apiStackAt(slot).isForeign()) return .TYPE_FOREIGN;
        if (vm.apiStackAt(slot).isList()) return .TYPE_LIST;
        if (vm.apiStackAt(slot).isMap()) return .TYPE_MAP;
        if (vm.apiStackAt(slot).isNull()) return .TYPE_NULL;
        if (vm.apiStackAt(slot).isString()) return .TYPE_STRING;
        return .TYPE_UNKNOWN;
    }

    pub fn getSlotBool(vm: *@This(), slot: usize) bool {
        vm.validateApiSlot(slot);
        Utils.assert(vm.apiStackAt(slot).isBool(), "Slot must hold a bool.");

        return vm.apiStackAt(slot).asBool();
    }

    pub fn getSlotBytes(vm: *@This(), slot: usize) []const u8 {
        vm.validateApiSlot(slot);
        Utils.assert(vm.apiStackAt(slot).isString(), "Slot must hold a string.");
        const string = vm.apiStackAt(slot).asString();
        return string.value;
    }

    pub fn getSlotDouble(vm: *@This(), slot: usize) f64 {
        vm.validateApiSlot(slot);
        Utils.assert(vm.apiStackAt(slot).isNum(), "Slot must hold a number.");
        return vm.apiStackAt(slot).asNum();
    }

    pub fn getSlotForeign(vm: *@This(), slot: usize) *anyopaque {
        vm.validateApiSlot(slot);
        Utils.assert(vm.apiStackAt(slot).isForeign(), "Slot must hold a foreign instance.");

        return vm.apiStackAt(slot).asForeign().data;
    }

    pub fn getSlotString(vm: *@This(), slot: usize) []const u8 {
        vm.validateApiSlot(slot);
        Utils.assert(vm.apiStackAt(slot).isString(), "Slot must hold a string.");
        return vm.apiStackAt(slot).asString().value;
    }

    pub fn getSlotHandle(vm: *@This(), slot: usize) *ZrenHandle {
        vm.validateApiSlot(slot);
        return vm.makeHandle(vm.apiStackAt(slot).*);
    }

    // 将[value]存储在foreign调用堆栈中的[slot]中.
    fn setSlot(vm: *@This(), slot: usize, value: Value) void {
        vm.validateApiSlot(slot);
        vm.apiStackAt(slot).* = value;
    }

    pub inline fn setSlotBool(vm: *@This(), slot: usize, value: bool) void {
        vm.setSlot(slot, if (value) .TRUE_VAL else .FALSE_VAL);
    }

    pub inline fn setSlotBytes(vm: *@This(), slot: usize, value: []const u8) void {
        vm.setSlot(slot, vm.newString(value));
    }

    pub inline fn setSlotDouble(vm: *@This(), slot: usize, value: f64) void {
        vm.setSlot(slot, Value.numToValue(value));
    }

    pub fn setSlotNewForeign(vm: *@This(), slot: usize, classSlot: usize, T: type) *T {
        vm.validateApiSlot(slot);
        vm.validateApiSlot(classSlot);
        Utils.assert(vm.apiStackAt(classSlot).isClass(), "Slot must hold a class.");

        const classObj = vm.apiStackAt(classSlot).asClass();
        Utils.assert(classObj.num_fields == null, "Class must be a foreign class.");

        const foreign = vm.newForeign(classObj, @sizeOf(T));
        vm.apiStackAt(slot).* = foreign.asObj().toVal();
        return foreign.cast(T);
    }

    pub inline fn setSlotNewList(vm: *@This(), slot: usize) void {
        vm.setSlot(slot, vm.newList(0).asObj().toVal());
    }

    pub inline fn setSlotNewMap(vm: *@This(), slot: usize) void {
        vm.setSlot(slot, vm.newMap().asObj().toVal());
    }

    pub inline fn setSlotNull(vm: *@This(), slot: usize) void {
        vm.setSlot(slot, .NULL_VAL);
    }

    pub inline fn setSlotString(vm: *@This(), slot: usize, text: []const u8) void {
        Utils.assert(text.len != 0, "String cannot be NULL.");
        vm.setSlot(slot, vm.newString(text));
    }

    pub inline fn setSlotHandle(vm: *@This(), slot: usize, handle: ?*ZrenHandle) void {
        Utils.assert(handle != null, "Handle cannot be NULL.");
        vm.setSlot(slot, handle.?.value);
    }

    pub fn getListCount(self: *@This(), listSlot: usize) usize {
        self.validateApiSlot(listSlot);
        Utils.assert(self.apiStackAt(listSlot).isList(), "Slot must hold a list.");

        return self.apiStackAt(listSlot).asList().elements.count;
    }

    pub fn getListElement(self: *@This(), listSlot: usize, index: isize, elementSlot: usize) void {
        self.validateApiSlot(listSlot);
        self.validateApiSlot(elementSlot);
        Utils.assert(self.apiStackAt(listSlot).isList(), "Slot must hold a list.");

        const elements = self.apiStackAt(listSlot).asList().elements;

        const usedIndex = validateIndexPure(elements.count, index) orelse @panic("Index out of bounds.");

        self.apiStackAt(elementSlot).* = elements.rat(usedIndex).*;
    }

    pub fn setListElement(self: *@This(), listSlot: usize, index: isize, elementSlot: usize) void {
        self.validateApiSlot(listSlot);
        self.validateApiSlot(elementSlot);
        Utils.assert(self.apiStackAt(listSlot).isList(), "Slot must hold a list.");

        const elements = self.apiStackAt(listSlot).asList().elements;

        const usedIndex = validateIndexPure(elements.count, index) orelse @panic("Index out of bounds.");

        elements.rat(usedIndex).* = self.apiStackAt(elementSlot).*;
    }

    pub fn insertInList(self: *@This(), listSlot: usize, index: isize, elementSlot: usize) void {
        self.validateApiSlot(listSlot);
        self.validateApiSlot(elementSlot);
        Utils.assert(self.apiStackAt(listSlot).isList(), "Must insert into a list.");

        const list = self.apiStackAt(listSlot).asList();

        // 负数索引从末尾开始计数.
        // 这里不使用validateIndex，因为insert允许比list.count大的索引.
        var i = index;
        if (i < 0) i += @intCast(list.elements.count + 1);

        Utils.assert(i <= list.elements.count, "Index out of bounds.");

        self.listInsert(list, self.apiStackAt(elementSlot).*, @intCast(i));
    }

    pub fn getMapCount(self: *@This(), slot: usize) usize {
        self.validateApiSlot(slot);
        Utils.assert(self.apiStackAt(slot).isMap(), "Slot must hold a map.");

        return self.apiStackAt(slot).asMap().count();
    }

    pub fn getMapContainsKey(self: *@This(), mapSlot: usize, keySlot: usize) bool {
        self.validateApiSlot(mapSlot);
        self.validateApiSlot(keySlot);
        Utils.assert(self.apiStackAt(mapSlot).isMap(), "Slot must hold a map.");

        const key = self.apiStackAt(keySlot).*;
        Utils.assert(key.isValidKey(), "Slot must hold a valid key.");
        if (!self.validateKey(key)) return false;

        return self.apiStackAt(mapSlot).asMap().mapContains(key);
    }

    pub fn getMapValue(self: *@This(), mapSlot: usize, keySlot: usize, valueSlot: usize) void {
        self.validateApiSlot(mapSlot);
        self.validateApiSlot(keySlot);
        self.validateApiSlot(valueSlot);
        Utils.assert(self.apiStackAt(mapSlot).isMap(), "Slot must hold a map.");

        const map = self.apiStackAt(mapSlot).asMap();
        const key = self.apiStackAt(keySlot).*;

        const value = map.mapGet(key);
        self.apiStackAt(valueSlot).* = if (value.isUndefined()) .NULL_VAL else value;
    }

    pub fn setMapValue(self: *@This(), mapSlot: usize, keySlot: usize, valueSlot: usize) void {
        self.validateApiSlot(mapSlot);
        self.validateApiSlot(keySlot);
        self.validateApiSlot(valueSlot);
        Utils.assert(self.apiStackAt(mapSlot).isMap(), "Must insert into a map.");

        const key = self.apiStackAt(keySlot).*;
        Utils.assert(key.isValidKey(), "Key must be a value type");

        if (!self.validateKey(key)) return;

        var map = self.apiStackAt(mapSlot).asMap();

        map.mapSet(key, self.apiStackAt(valueSlot).*);
    }

    pub fn removeMapValue(self: *@This(), mapSlot: usize, keySlot: usize, removedValueSlot: usize) void {
        self.validateApiSlot(mapSlot);
        self.validateApiSlot(keySlot);
        Utils.assert(self.apiStackAt(mapSlot).isMap(), "Slot must hold a map.");

        const key = self.apiStackAt(keySlot).*;
        if (!self.validateKey(key)) return;

        const map = self.apiStackAt(mapSlot).asMap();
        const removed = self.mapRemoveKey(map, key);
        self.setSlot(removedValueSlot, removed);
    }

    // 从module中获取变量[name], 将其存储在slot中.
    pub fn getVariable(self: *@This(), module: []const u8, name: []const u8, slot: usize) void {
        Utils.assert(module.len != 0, "Module cannot be NULL.");
        Utils.assert(name.len != 0, "Variable name cannot be NULL.");

        const moduleName = self.stringFormat("$", .{module});
        self.pushRoot(moduleName.asObj());

        const moduleObj = self.getModule(moduleName) orelse @panic("Could not find module.");

        self.popRoot();
        const variableSlot = self.symbolTableFind(&moduleObj.variableNames, name) orelse @panic("Could not find variable.");

        self.setSlot(slot, moduleObj.variables.at(variableSlot));
    }

    pub fn hasVariable(self: *@This(), module: []const u8, name: []const u8) bool {
        Utils.assert(module.len != 0, "Module cannot be NULL.");
        Utils.assert(name.len != 0, "Variable name cannot be NULL.");

        const moduleName = self.stringFormat("$", .{module});
        self.pushRoot(moduleName.asObj());

        // 这里不使用hasModule, 因为需要使用模块对象.
        const moduleObj = self.getModule(moduleName) orelse @panic("Could not find module.");

        self.popRoot();

        const variableSlot = self.symbolTableFind(&moduleObj.variableNames, name);

        return variableSlot != null;
    }

    pub fn hasModule(self: *@This(), module: []const u8) bool {
        Utils.assert(module.len > 0, "Module cannot be NULL.");

        const moduleName = self.stringFormat("$", .{module});
        self.pushRoot(moduleName.asObj());

        const moduleObj = self.getModule(moduleName);

        self.popRoot();

        return moduleObj != null;
    }

    // 通过 [name] 查找已加载的模块. 未找到时返回 null.
    pub fn getModule(self: *@This(), name: Value) ?*ObjModule {
        var module_value: Value = self.mapGet(self.modules, name);
        return if (!module_value.isUndefined()) module_value.asModule() else null;
    }

    pub fn abortFiber(self: *@This(), slot: usize) void {
        self.validateApiSlot(slot);
        self.fiber.?.err = self.apiStackAt(slot).*;
    }

    pub inline fn getUserData(self: *@This()) ?*anyopaque {
        return self.config.userData;
    }

    pub inline fn setUserData(self: *@This(), data: *anyopaque) void {
        self.config.userData = data;
    }

    // 用method not found 错误终止classObj的symbol方法.
    fn methodNotFound(vm: *@This(), classObj: *ObjClass, symbol: usize) void {
        vm.fiber.?.err = vm.stringFormat("@ does not implement '$'.", .{ classObj.name.asObj().toVal(), vm.methodNames.at(symbol).value });
    }

    pub inline fn mapGet(_: *@This(), map: *ObjMap, key: Value) Value {
        return map.mapGet(key);
    }

    pub inline fn mapSet(_: *@This(), map: *ObjMap, key: Value, value: Value) void {
        map.mapSet(key, value);
    }

    pub fn mapRemoveKey(self: *@This(), map: *ObjMap, key: Value) Value {
        // TODO 不知道是否需要优化效率
        const value = self.mapGet(map, key);
        if (value.isUndefined()) return .NULL_VAL;
        if (value.isObj()) self.pushRoot(value.asObj());

        _ = map.removeKey(key);
        if (map.entries.count() == 0) map.clear();

        if (value.isObj()) self.popRoot();
        return value;
    }

    pub fn newVM(cfg: ?ZrenConfiguration) ZrenVM {
        var config: ZrenConfiguration = cfg orelse .DEFAULT_CONFIG;
        config.allocator = config.allocator orelse std.heap.c_allocator;
        var vm: ZrenVM = .{
            .config = config,
            .rawAllocator = config.allocator.?,
            .nextGC = config.initialHeapSize,
        };
        vm.allocator = GcAllocator.init(config.allocator.?, &vm); // TODO 这里需要优化, 逻辑太怪了
        vm.gray = std.ArrayList(*Obj).init(vm.allocator);

        vm.gray.ensureTotalCapacity(4) catch unreachable;
        vm.nextGC = vm.config.initialHeapSize;

        vm.methodNames = SymbolTable.init(vm.allocator);

        vm.modules = vm.newMap();
        vm.initializeCore();
        return vm;
    }

    pub fn newList(self: *@This(), eleCount: usize) *ObjList {
        var list = self.allocator.create(ObjList) catch unreachable;
        self.initObj(list.asObj(), .OBJ_LIST, self.listClass);
        list.elements = ValueBuffer.init(self.allocator);
        list.elements.resize(eleCount);
        return list;
    }

    pub fn listInsert(self: *@This(), list: *ObjList, value: Value, index: u32) void {
        if (value.isObj()) self.pushRoot(value.asObj());
        list.elements.push(.NULL_VAL);
        if (value.isObj()) self.popRoot();
        list.elements.data.insert(index, value) catch unreachable;
    }

    pub fn listIndexOf(self: *@This(), list: *ObjList, value: Value) ?u32 {
        _ = self; // TODO 改进>>>>>
        for (0..list.elements.count) |i| {
            if (list.elements.rat(i).eql(value)) return @intCast(i);
        }
        return null;
    }

    pub fn listRemoveAt(self: *@This(), list: *ObjList, index: u32) Value {
        var removed = list.elements.at(index);
        if (removed.isObj()) self.pushRoot(removed.asObj());
        // 这里重设removed, 因为移除后, 原removed会被覆盖(Value类型会被重置, 和Obj不同)
        removed = list.elements.data.orderedRemove(index);
        // TODO shrink???
        if (removed.isObj()) self.popRoot();
        list.elements.count -= 1;
        return removed;
    }

    pub fn newMap(self: *@This()) *ObjMap {
        var modules = self.allocator.create(ObjMap) catch unreachable;
        self.initObj(modules.asObj(), .OBJ_MAP, self.mapClass);
        modules.entries = V.ValueHashMap.init(self.allocator);
        return modules;
    }

    pub fn newFiber(self: *@This(), closure: ?*ObjClosure) *ObjFiber {
        const stackCapacity = if (closure) |c| Utils.powerOf2Ceil(c.func.maxSlots + 1) else 1;

        const fiber = self.allocator.create(ObjFiber) catch unreachable;
        fiber.allocator = self.allocator;

        self.initObj(fiber.asObj(), .OBJ_FIBER, self.fiberClass);

        fiber.initStack(stackCapacity);
        fiber.loadStack(0); // 代表从栈底开始

        fiber.initFrames(Constants.C.INITIAL_CALL_FRAMES);

        fiber.openUpvalues = null;
        fiber.caller = null;
        fiber.err = .NULL_VAL;
        fiber.state = .FIBER_OTHER;

        if (closure) |c| {
            fiber.pushFrame(c, 0);
            fiber.push(c.asObj().toVal());
        }

        return fiber;
    }

    pub fn newForeign(self: *@This(), classObj: *ObjClass, size: usize) *ObjForeign {
        const object = self.allocator.create(ObjForeign) catch unreachable;
        self.initObj(object.asObj(), .OBJ_FOREIGN, classObj);
        object.allocator = &self.allocator;
        object.allocData(size);
        return object;
    }

    pub fn newFunction(self: *@This(), module: ?*ObjModule, maxSlots: usize) *ObjFunc {
        const debug = self.allocator.create(FuncDebug) catch unreachable;
        debug.name = &.{};
        debug.source_lines = UsizeBuffer.init(self.allocator);

        const func = self.allocator.create(ObjFunc) catch unreachable;
        self.initObj(func.asObj(), .OBJ_FUNC, self.fnClass);

        func.constants = ValueBuffer.init(self.allocator);
        func.code = ByteBuffer.init(self.allocator);
        func.module = module;
        func.maxSlots = maxSlots;
        func.numUpvalues = 0;
        func.arity = 0;
        func.debug = debug;

        return func;
    }

    pub fn freeObj(self: *@This(), obj: *Obj) void {
        comptime if (builtin.mode == .Debug) {
            // TODO
            // #if WREN_DEBUG_TRACE_MEMORY
            // printf("free ");
            // obj->DumpValue();
            // printf(" @ %p\n", obj);
            // #endif
        };

        switch (obj.obj_type) {
            .OBJ_CLASS => obj.asClass().methods.clear(),

            .OBJ_FIBER => {
                const fiber = obj.asFiber();
                fiber.rawFrames.clear();
                fiber.stack.clear();
            },

            .OBJ_FUNC => {
                const func = obj.asFunc();
                func.constants.clear();
                func.code.clear();
                func.debug.source_lines.clear();
            },

            .OBJ_FOREIGN => self.finalizeForeign(obj.asForeign()),

            .OBJ_LIST => obj.asList().elements.clear(),

            .OBJ_MAP => obj.asMap().entries.clearAndFree(),

            .OBJ_MODULE => {
                obj.asModule().variableNames.clear();
                obj.asModule().variables.clear();
            },

            .OBJ_CLOSURE,
            .OBJ_INSTANCE,
            .OBJ_RANGE,
            .OBJ_STRING,
            .OBJ_UPVALUE,
            => {},
        }
        self.allocator.destroy(obj);
    }

    pub inline fn getClassInline(self: *@This(), value: Value) ?*ObjClass {
        if (build_options.nan_tagging) {
            if (value.isNum()) return self.numClass;
            if (value.isObj()) return value.asObj().class_obj;
            switch (value.getTag()) {
                Value.TAG_FALSE => return self.boolClass,
                Value.TAG_NAN => return self.numClass,
                Value.TAG_NULL => return self.nullClass,
                Value.TAG_TRUE => return self.boolClass,
                else => return null,
            }
        } else {
            switch (value.value_type) {
                .VAL_FALSE => return self.boolClass,
                .VAL_NULL => return self.nullClass,
                .VAL_NUM => return self.numClass,
                .VAL_TRUE => return self.boolClass,
                .VAL_OBJ => return value.asObj().class_obj,
                .VAL_UNDEFINED => unreachable,
            }
        }
        return null;
    }

    pub fn getClass(self: *@This(), value: Value) ?*ObjClass {
        return self.getClassInline(value);
    }

    pub fn getModuleVar(self: *@This(), module: *ObjModule, variableName: *Value) Value {
        if (self.symbolTableFind(&module.variableNames, variableName.asString().value)) |i| {
            return module.variables.at(i);
        }
        // const variableEntry = module.variableNames.index(variableName.asString());
        // if (variableEntry) |i| {
        //     return module.variables.rat(i).*;
        // }
        var module_name = module.name.?;
        self.fiber.?.err = self.stringFormat("Could not find a variable named '@' in module '@'.", .{ variableName, module_name.asObj().toVal() });
        return Value.NULL_VAL;
    }

    pub fn numToString(self: *@This(), value: f64) Value {
        if (std.math.isNan(value)) return self.newString("nan");
        if (std.math.isInf(value)) {
            if (value > 0) return self.newString("infinity") else return self.newString("-infinity");
        }

        // 使用足够大的缓冲区将双精度浮点转为string(对齐c中"%.14g"). 例如:
        //
        //     -1.12345678901234e-1022
        //
        // 我们得到:
        //
        // + 1 char 用于符号
        // + 1 char 用于数字1
        // + 1 char 用于 "."
        // + 14 chars 用于小数
        // + 1 char 用于 "e"
        // + 1 char 用于科学计数的 "-" 或 "+"
        // + 4 chars 科学计数的指数
        // + 1 char 用于终止符 "\0"
        // = 24
        var buf = [_]u8{0} ** 256;
        // TODO 这里可能有问题
        const rbuf = Utils.fmt.formatFloat14g(&buf, value) catch unreachable;
        // const rbuf = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable;
        return self.newString(rbuf);
    }

    pub inline fn stringFromCodePoint(self: *@This(), value: u32) Value {
        var buf = [_]u8{0} ** 4;
        const len = Utils.utf8Encode(value, buf[0..]);
        return self.newString(buf[0..len]);
    }

    pub inline fn stringFromByte(self: *@This(), value: u8) Value {
        return self.newString(&.{value});
    }

    pub fn stringFormat(self: *@This(), comptime format: []const u8, args: anytype) Value {
        comptime var i: usize = 0;
        var totalLength: usize = 0;

        inline for (format) |c| {
            switch (c) {
                '$' => {
                    totalLength += args[i].len;
                    i += 1;
                },
                '@' => {
                    totalLength += args[i].asString().value.len;
                    i += 1;
                },
                else => totalLength += 1,
            }
        }

        const content = self.allocator.alloc(u8, totalLength) catch unreachable;

        i = 0;
        var s = content.ptr;
        inline for (format) |c| {
            switch (c) {
                '$' => {
                    const string: []const u8 = args[i];
                    std.mem.copyForwards(u8, s[0..string.len], string);
                    s += string.len;
                    i += 1;
                },
                '@' => {
                    const string: *ObjString = args[i].asString();
                    std.mem.copyForwards(u8, s[0..string.value.len], string.value);
                    s += string.value.len;
                    i += 1;
                },
                else => {
                    s[0] = c;
                    s += 1;
                },
            }
        }
        var result = self.newString(content);
        return result.asObj().toVal();
    }

    pub fn stringCodePointAt(self: *@This(), string: *ObjString, index: usize) Value {
        Utils.assert(index < string.value.len, "Index out of bounds.");
        if (Utils.utf8Decode(string.value[index..])) |cp| {
            return self.stringFromCodePoint(@intCast(cp));
        }
        // 如果不是一个有效的UTF-8序列, 将其视为单个原始字节
        return self.newString(&.{string.value[index]});
    }

    pub fn initObj(self: *@This(), obj: *Obj, obj_type: ObjType, class: ?*ObjClass) void {
        obj.obj_type = obj_type;
        obj.is_dark = false;
        obj.class_obj = class orelse undefined;
        obj.next = self.first;
        self.first = obj;
    }

    fn finalizeForeign(self: *@This(), foreign: *ObjForeign) void {
        const symbol = self.symbolTableFind(&self.methodNames, "<finalize>") orelse @panic("");

        const classObj = foreign.asObj().class_obj;
        if (symbol >= classObj.methods.count) return;

        const method = classObj.methods.rat(symbol);
        if (method.method_type == .METHOD_NONE) return;

        Utils.assert(method.method_type == .METHOD_FOREIGN, "Finalize method must be foreign.");

        const finalizer = method.as.foreign orelse return;
        finalizer(foreign.cast(ZrenVM));
    }

    // Let the host resolve an imported module name if it wants to.
    fn resolveModulePath(vm: *@This(), name: Value) Value {
        // TODO 相关模块加载 和 字符串的拷贝 释放  需要优化
        // 如果host不关心resolve, 则直接返回name
        const r = vm.config.resolveModuleFn orelse return name;

        const fiber = vm.fiber orelse unreachable;
        const func = fiber.frames[fiber.numFrames - 1].closure.func;
        const importer = func.module.?.name.?;
        var resolved = [_]u8{0} ** 256;

        const len = r(vm, importer.value, name.asString().value, resolved[0..]);
        if (len == 0) {
            vm.fiber.?.err = vm.stringFormat("Could not resolve module '@' imported from '@'.", .{ name, importer.asObj().toVal() });
            return .NULL_VAL;
        }

        // 拷贝字符串到ObjString
        const resName = vm.newString(resolved[0..len]);
        return resName;
    }

    pub fn importModule(self: *@This(), inNname: *Value) Value {
        var name = self.resolveModulePath(inNname.*);
        // 如果模块已经加载, 则不需要做任何事情
        const existing = self.modules.mapGet(name);
        if (!existing.isUndefined()) return existing;
        self.pushRoot(name.asObj());

        var result: ZrenLoadModuleResult = .{};
        // defer self.allocator.free(result.source); // TODO 内存清理在onComplete中进项??
        if (self.config.loadModuleFn) |f| result = f(self, name.asString().value);

        // 如果没提供source, 检查是否是内置的可选模块.
        if (result.source.len == 0) {
            result.onComplete = null;
            const nameString = name.asString();
            if (std.mem.eql(u8, nameString.value, "meta")) result.source = Constants.metaModuleSource;
            if (std.mem.eql(u8, nameString.value, "random")) result.source = Constants.randomModuleSource;
        }

        if (result.source.len == 0) {
            self.fiber.?.err = self.stringFormat("Could not load module '@'.", .{name});
            self.popRoot();
            return .NULL_VAL;
        }

        const moduleCLosure = self.compileInModule(&name, result.source, false, true);
        // TODO result.source 释放 这里需要改进
        if (result.onComplete) |f| f(self, name.asString().value, result);

        if (moduleCLosure == null) {
            self.fiber.?.err = self.stringFormat("Could not compile module '@'.", .{name});
            self.popRoot();
            return .NULL_VAL;
        }

        self.popRoot();

        return moduleCLosure.?.asObj().toVal();
    }

    pub fn functionBindName(self: *@This(), func: ?*ObjFunc, name: []const u8) void {
        func.?.debug.name = self.allocator.alloc(u8, name.len + 1) catch unreachable;
        std.mem.copyForwards(u8, func.?.debug.name, name);
        func.?.debug.name[name.len] = '\x00';
        // memcpy(fn->debug->name, name, length);
        // fn->debug->name[length] = '\0';
    }

    pub fn newModule(self: *@This(), name: ?*ObjString) *ObjModule {
        const module = self.allocator.create(ObjModule) catch unreachable;

        // 模块不会作为第一类对象使用, 所以不需要类
        self.initObj(module.asObj(), .OBJ_MODULE, null);

        self.pushRoot(module.asObj());
        module.variableNames = SymbolTable.init(self.allocator);
        module.variables = ValueBuffer.init(self.allocator);

        module.name = name;

        self.popRoot();
        return module;
    }

    pub fn newRange(self: *@This(), from: f64, to: f64, isInclusive: bool) Value {
        var range = self.allocator.create(ObjRange) catch unreachable;
        self.initObj(range.asObj(), .OBJ_RANGE, self.rangeClass);
        range.from = from;
        range.to = to;
        range.isInclusive = isInclusive;
        return range.asObj().toVal();
    }

    pub fn newInstance(self: *@This(), class: *ObjClass) Value {
        var instance = self.allocator.create(ObjInstance) catch unreachable;
        self.initObj(instance.asObj(), .OBJ_INSTANCE, class);
        instance.fields = self.allocator.alloc(Value, class.num_fields.?) catch unreachable;
        for (0..class.num_fields.?) |i| {
            instance.fields[i] = .NULL_VAL;
        }
        return instance.asObj().toVal();
    }

    pub fn newClosure(self: *@This(), func: *ObjFunc) *ObjClosure {
        const closure = self.allocator.create(ObjClosure) catch unreachable;
        self.initObj(closure.asObj(), .OBJ_CLOSURE, self.fnClass);
        closure.func = func;
        closure.upvalues = self.allocator.alloc(?*ObjUpvalue, func.numUpvalues) catch unreachable;
        for (0..func.numUpvalues) |i| closure.upvalues[i] = undefined;
        return closure;
    }

    pub fn newUpvalue(self: *@This(), value: *Value, index: usize) *ObjUpvalue {
        const upvalue = self.allocator.create(ObjUpvalue) catch unreachable;
        self.initObj(upvalue.asObj(), .OBJ_UPVALUE, undefined);
        upvalue.value = value;
        upvalue.fromStackIndex = index;
        upvalue.closed = .NULL_VAL;
        upvalue.next = null;
        return upvalue;
    }

    pub fn newClass(self: *@This(), superclass: *ObjClass, num_fields: ?usize, name: *ObjString) *ObjClass {
        var metaclassName = self.stringFormat("@ metaclass", .{name.asObj()});
        self.pushRoot(metaclassName.asObj());

        const metaclass = self.newSingleClass(0, metaclassName.asString());

        metaclass.asObj().class_obj = self.classClass;
        self.popRoot();
        self.pushRoot(metaclass.asObj());
        self.bindSuperclass(metaclass, self.classClass);

        const classObj = self.newSingleClass(num_fields, name);

        self.pushRoot(classObj.asObj());
        classObj.asObj().class_obj = metaclass;
        self.bindSuperclass(classObj, superclass);
        self.popRoot();
        self.popRoot();
        return classObj;
    }

    pub fn newSingleClass(self: *@This(), num_fields: ?usize, name: *ObjString) *ObjClass {
        var classObj = self.allocator.create(ObjClass) catch unreachable;
        self.initObj(classObj.asObj(), .OBJ_CLASS, null);
        classObj.super_class = null;
        classObj.num_fields = num_fields;
        classObj.name = name;
        classObj.attributes = .NULL_VAL;

        self.pushRoot(classObj.asObj());
        classObj.methods = MethodBuffer.init(self.allocator);
        self.popRoot();

        return classObj;
    }

    pub fn newStringFromRange(self: *@This(), source: *ObjString, start: usize, count: usize, step: i32) Value {
        const from = source.value;
        var length: usize = 0;
        for (0..count) |i| {
            const index = Utils.calcRangeIndex(start, i, step);
            length += Utils.utf8DecodeNumBytes(from[index]);
        }

        const content = self.allocator.alloc(u8, length) catch unreachable;
        var to = content.ptr;
        for (0..count) |i| {
            const index = Utils.calcRangeIndex(start, i, step);
            const codePoint = Utils.utf8Decode(from[@intCast(index)..]) orelse continue;
            to += Utils.utf8Encode(codePoint, to);
        }

        var result = self.allocator.create(ObjString) catch unreachable;
        self.initObj(result.asObj(), .OBJ_STRING, self.stringClass);
        result.value = content;
        result.doHash();

        return result.asObj().toVal();
    }

    pub fn newString(self: *@This(), name: []const u8) Value {
        var s = self.allocator.create(ObjString) catch unreachable;
        self.initObj(s.asObj(), .OBJ_STRING, self.stringClass);
        s.value = self.allocator.dupe(u8, name) catch unreachable;
        s.doHash();
        return s.asObj().toVal();
    }

    fn popGray(self: *@This()) ?*Obj {
        return self.gray.pop();
    }

    fn pushGray(self: *@This(), obj: *Obj) void {
        self.gray.append(obj) catch unreachable;
    }

    fn grayObj(self: *@This(), o: *Obj) void {
        if (o.is_dark) return; // 若已标记则返回
        o.is_dark = true; //      标记为黑色(已访问)
        self.pushGray(o); //      将其添加到灰名单中，以便稍后递归地探索更多标记
    }

    fn grayValue(self: *@This(), v: Value) void {
        if (v.isObj()) self.grayObj(v.asObj()) else return;
    }

    fn grayBuffer(self: *@This(), buffer: *ValueBuffer) void {
        for (buffer.data.items) |v| self.grayValue(v);
    }

    fn blackenObjs(self: *@This()) void {
        while (self.gray.pop()) |obj| {
            self.blackenObj(obj);
        }
    }

    fn blackenObj(self: *@This(), obj: *Obj) void {
        comptime if (builtin.mode == .Debug) {
            // TODO
            //   #if WREN_DEBUG_TRACE_MEMORY
            //     printf("mark ");
            //     obj->DumpValue();
            //     printf(" @ %p\n", obj);
            //   #endif
        };
        switch (obj.obj_type) {
            .OBJ_CLASS => self.blackenClass(obj.asClass()),
            .OBJ_CLOSURE => self.blackenClosure(obj.asClosure()),
            .OBJ_FIBER => self.blackenFiber(obj.asFiber()),
            .OBJ_FUNC => self.blackenFunc(obj.asFunc()),
            .OBJ_FOREIGN => self.blackenForeign(obj.asForeign()),
            .OBJ_INSTANCE => self.blackenInstance(obj.asInstance()),
            .OBJ_LIST => self.blackenList(obj.asList()),
            .OBJ_MAP => self.blackenMap(obj.asMap()),
            .OBJ_MODULE => self.blackenModule(obj.asModule()),
            .OBJ_RANGE => self.blackenRange(obj.asRange()),
            .OBJ_STRING => self.blackenString(obj.asString()),
            .OBJ_UPVALUE => self.blackenUpvalue(obj.asUpvalue()),
        }
    }

    fn blackenClass(self: *@This(), classObj: *ObjClass) void {
        self.grayObj(classObj.asObj().class_obj.asObj()); // 元类
        if (classObj.super_class) |sc| self.grayObj(sc.asObj()); // 超类

        for (0..classObj.methods.count) |i| {
            if (classObj.methods.rat(i).method_type == .METHOD_BLOCK) {
                self.grayObj(classObj.methods.rat(i).as.closure.asObj());
            }
        }

        self.grayObj(classObj.name.asObj());
        if (!classObj.attributes.isNull()) self.grayObj(classObj.attributes.asObj());

        // 保持追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjClass);
        self.bytesAllocated += classObj.methods.data.capacity * @sizeOf(Method);
    }

    fn blackenClosure(self: *@This(), closure: *ObjClosure) void {
        self.grayObj(closure.func.asObj()); // 标记function.

        // 标记upvalue
        for (closure.upvalues) |i| if (i) |up| self.grayObj(up.asObj());

        // 保持追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjClosure);
        self.bytesAllocated += @sizeOf(*ObjUpvalue) * closure.upvalues.len;
    }

    fn blackenFiber(self: *@This(), fiber: *ObjFiber) void {
        // 栈函数
        for (0..fiber.numFrames) |i| {
            self.grayObj(fiber.frames[i].closure.asObj());
        }

        // 栈变量
        for (0..fiber.stack.top) |i| {
            self.grayValue(fiber.stack.buffer[i]);
        }

        // 开放的upvalue
        var upvalue = fiber.openUpvalues;
        while (upvalue) |u| : (upvalue = u.next) self.grayObj(u.asObj());

        // 调用者
        if (fiber.caller) |c| self.grayObj(c.asObj());
        self.grayValue(fiber.err);
        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjFiber);
        self.bytesAllocated += fiber.rawFrames.allocatedSize();
        self.bytesAllocated += fiber.stack.allocatedSize();
    }

    fn blackenFunc(self: *@This(), func: *ObjFunc) void {
        // 标记常量
        self.grayBuffer(&func.constants);
        // 标记它所属的模块, 以防它已被卸载
        if (func.module) |m| self.grayObj(m.asObj());

        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjFunc);
        self.bytesAllocated += func.code.allocatedSize();
        self.bytesAllocated += func.constants.allocatedSize();

        // 调试行号缓冲区
        self.bytesAllocated += @sizeOf(u8) * func.debug.name.len;
        // TODO: What about the function name?
    }

    fn blackenForeign(self: *@This(), foreign: *ObjForeign) void {
        _ = .{ self, foreign }; // TODO
        // TODO: Keep track of how much memory the foreign object uses.
        // We can store this in each foreign object, but it will balloon the size.
        // We may not want that much overhead.
        // One option would be to let the foreign class register a C function that returns a size for the object.
        // That way the VM doesn't always have to explicitly store it.
    }

    fn blackenInstance(self: *@This(), instance: *ObjInstance) void {
        self.grayObj(instance.asObj().class_obj.asObj());
        // 标记实例的字段
        for (0..instance.asObj().class_obj.num_fields.?) |i| {
            self.grayValue(instance.fields[i]);
        }
        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjInstance);
        self.bytesAllocated += @sizeOf(Value) * instance.fields.len;
    }

    fn blackenList(self: *@This(), list: *ObjList) void {
        // 标记元素
        self.grayBuffer(&list.elements);
        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjList);
        self.bytesAllocated += list.allocatedSize();
    }

    fn blackenMap(self: *@This(), map: *ObjMap) void {
        // 标记entry
        var iter = map.iterator();
        while (iter.next()) |entry| {
            if (entry.key_ptr.isUndefined()) continue;
            self.grayValue(entry.key_ptr.*);
            self.grayValue(entry.value_ptr.*);
        }

        self.bytesAllocated += @sizeOf(ObjMap);
        self.bytesAllocated += map.allocatedSize();
    }

    fn blackenModule(self: *@This(), module: *ObjModule) void {
        // 顶层变量
        for (0..module.variables.count) |i| {
            self.grayValue(module.variables.at(i));
        }

        self.blackenSymbolTable(&module.variableNames);
        if (module.name) |n| self.grayObj(n.asObj());

        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjModule);
    }

    // 将符号表中的所有符号标记为未使用
    fn blackenSymbolTable(self: *@This(), table: *SymbolTable) void {
        for (0..table.count) |i| self.grayObj(table.at(i).asObj());
        // 追踪仍在使用的内存量
        self.bytesAllocated += table.allocatedSize();
    }

    fn blackenRange(self: *@This(), range: *ObjRange) void {
        _ = range;
        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjRange);
    }

    fn blackenString(self: *@This(), string: *ObjString) void {
        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjString) + string.value.len;
    }

    fn blackenUpvalue(self: *@This(), upvalue: *ObjUpvalue) void {
        // 标记关闭的对象（以防它被关闭）
        self.grayValue(upvalue.closed);
        // 追踪仍在使用的内存量
        self.bytesAllocated += @sizeOf(ObjUpvalue);
    }

    pub fn allocateString(self: *@This(), length: usize) *ObjString {
        const stringObj = self.allocator.create(ObjString) catch unreachable;
        self.initObj(stringObj.asObj(), .OBJ_STRING, self.stringClass);
        stringObj.value = self.allocator.alloc(u8, length) catch unreachable;
        return stringObj;
    }

    pub fn createForeign(self: *@This(), fiber: *ObjFiber, classObj: *ObjClass) void {
        Utils.assert(classObj.num_fields == null, "Class must be a foreign class.");
        const find_symbol = self.symbolTableFind(&self.methodNames, "<allocate>");

        const symbol = find_symbol orelse @panic("Should have defined <allocate> symbol.");

        Utils.assert(classObj.methods.count > symbol, "Class should have allocator.");

        const method = classObj.methods.rat(symbol);

        Utils.assert(method.method_type == .METHOD_FOREIGN, "Allocator should be foreign.");
        Utils.assert(self.isApiStackNull(), "Cannot already be in foreign call.");

        self.setApiStack(fiber.stack.buffer, self.fiber.?.frame.stackOffset);
        method.as.foreign.?(self);
        self.clearApiStack();
    }

    pub fn endClass(self: *@This()) void {
        const attributes = self.fiber.?.peek(2);
        const classValue = self.fiber.?.peek(1);
        classValue.asClass().attributes = attributes.*;
        self.fiber.?.stack.nOffset(2);
    }

    pub fn createClass(self: *@This(), num_fields: ?usize, module: ?*ObjModule) void {
        const fiber = self.fiber orelse unreachable;
        // 从栈中获取类名和父类
        const name = fiber.peek(2);
        const superclass = fiber.peek(1);

        fiber.stack.nOffset(1); // 栈上有两个值，留下一个，丢弃另一个槽位

        fiber.err = self.validateSuperclass(name, superclass, num_fields);
        if (fiber.hasErr()) return;

        const classObj = self.newClass(superclass.asClass(), num_fields, name.asString());
        fiber.stack.peek(1).* = classObj.asObj().toVal();

        // 字段数为null, 说明是foreign class
        if (num_fields == null) self.bindForeignClass(classObj, module);
    }

    pub fn validateSuperclass(self: *@This(), name: *Value, superclassValue: *Value, num_fields: ?usize) Value {
        // 确保父类是一个类
        if (!superclassValue.isClass()) {
            return self.stringFormat("Class '@' cannot inherit from a non-class object.", .{name});
        }

        // 确保它不继承自一个密封的内置类型.
        // 这些类上的primitive 方法假定实例是其他 Obj___ 类型之一, 如果它实际上是 ObjInstance, 则将会失败
        const superclass = superclassValue.asClass();
        if (superclass == self.classClass or
            superclass == self.fiberClass or
            superclass == self.fnClass or // 包含 OBJ_CLOSURE.
            superclass == self.listClass or
            superclass == self.mapClass or
            superclass == self.rangeClass or
            superclass == self.stringClass or
            superclass == self.boolClass or
            superclass == self.nullClass or
            superclass == self.numClass)
        {
            return self.stringFormat("Class '@' cannot inherit from built-in class '@'.", .{ name, superclass.name.asObj().toVal() });
        }

        const s_num_fields = superclass.num_fields orelse {
            return self.stringFormat("Class '@' cannot inherit from foreign class '@'.", .{ name, superclass.name.asObj().toVal() });
        };

        if (num_fields == null and s_num_fields > 0) {
            return self.stringFormat("Foreign class '@' may not inherit from a class with fields.", .{name});
        }
        // TODO 这里的逻辑需要改进
        if ((s_num_fields + if (num_fields) |n| n else 0) > Constants.C.MAX_FIELDS) {
            return self.stringFormat("Class '@' may not have more than 255 fields, including inherited ones.", .{name});
        }
        // if ((s_num_fields + (if (num_fields) |n| n else @as(u64, @bitCast(@as(i64, -1))))) > Constants.C.MAX_FIELDS) {
        //     return self.stringFormat("Class '@' may not have more than 255 fields, including inherited ones.", .{name});
        // }

        return .NULL_VAL;
    }

    pub fn bindSuperclass(self: *@This(), insubclass: ?*ObjClass, superclass: *ObjClass) void {
        const subclass = insubclass orelse @panic("Must have superclass.");
        subclass.super_class = superclass;
        // 将superclass的num_fields加到总num_fields中
        if (subclass.num_fields) |*num_fields| {
            num_fields.* += superclass.num_fields.?;
        } else {
            Utils.assert(superclass.num_fields.? == 0, "A foreign class cannot inherit from a class with fields.");
        }

        // 从父类继承方法
        for (0..superclass.methods.count) |i| {
            self.bindMethod(subclass, i, superclass.methods.at(i));
        }
    }

    pub fn bindForeignClass(self: *@This(), classObj: *ObjClass, module: ?*ObjModule) void {
        var methods: ZrenForeignClassMethods = .{};
        if (self.config.bindForeignClassFn) |bindForeignClassFn| {
            methods = bindForeignClassFn(self, module.?.name.?.value, classObj.name.value) orelse .{};
        }
        if (methods.allocate == null and methods.finalize == null) {
            if (std.mem.eql(u8, module.?.name.?.value, "random")) {
                methods = self.randomBindForeignClass(module.?.name.?.value, classObj.name.value);
            }
        }

        var method: Method = .{ .method_type = .METHOD_FOREIGN };
        var symbol = self.symbolTableEnsure(&self.methodNames, "<allocate>");
        if (methods.allocate != null) {
            method.as = .{ .foreign = methods.allocate };
            self.bindMethod(classObj, symbol, method);
        }

        symbol = self.symbolTableEnsure(&self.methodNames, "<finalize>");
        if (methods.finalize != null) {
            method.as = .{ .foreign = methods.finalize };
            self.bindMethod(classObj, symbol, method);
        }
    }

    pub fn gBindMethod(self: *@This(), methodType: OpCode, symbol: usize, module: *ObjModule, inClassObj: *ObjClass, methodValue: Value) void {
        var classObj = inClassObj;
        const className = classObj.name.value;
        if (methodType == .CODE_METHOD_STATIC) classObj = classObj.asObj().class_obj;

        var method: Method = .{};
        if (methodValue.isString()) {
            const name = methodValue.asString().value;
            method.method_type = .METHOD_FOREIGN;
            const foreign = self.findForeignMethod(
                module.name.?.value,
                className,
                methodType == .CODE_METHOD_STATIC,
                name,
            );
            method.as = .{ .foreign = foreign };
            if (method.as.foreign == null) {
                self.fiber.?.err = self.stringFormat("Could not find foreign method '@' for class $ in module '$'.", .{ methodValue, classObj.name.value, module.name.?.value });
                return;
            }
        } else {
            method.as = .{ .closure = methodValue.asClosure() };
            method.method_type = .METHOD_BLOCK;
            // 这里已经知道了superclass, 进行字节码填充
            classObj.bindMethodCode(method.as.closure.func);
        }

        self.bindMethod(classObj, symbol, method);
    }

    pub fn bindMethod(self: *@This(), classObj: *ObjClass, symbol: usize, method: Method) void {
        _ = self;
        // TODO 改进2: class的方法绑定时 symbol 都是从vm的methodNames中获取, 导致class自己的methods膨胀
        // TODO 这里可以改进
        if (symbol >= classObj.methods.count) {
            const noMethod: Method = .{ .method_type = .METHOD_NONE };
            classObj.methods.fill(noMethod, symbol - classObj.methods.count + 1);
        }
        classObj.methods.rat(symbol).* = method; // 指定写入到这个位置(symbol)
    }

    // 验证[value]是位于`[0, count)`的整数
    // 允许负索引, 相当于从末尾开始. 返回有效的正索引值.
    // 如果无效, 报告错误并返回null.
    pub fn validateIndexValue(self: *@This(), count: usize, arg: f64, name: []const u8) ?u32 {
        // TODO 可能需要使用usize实现
        if (!self.validateIntValue(arg, name)) return null;

        var value: isize = @intFromFloat(arg);
        // 负索引: 从末尾开始
        if (value < 0) value += @intCast(count);
        // 边界检查
        if (value >= 0 and value < count) return @intCast(value);

        self.fiber.?.err = self.stringFormat("$ out of bounds.", .{name});
        return null;
    }

    pub inline fn validateIndexPure(count: usize, index: isize) ?u32 {
        // TODO 可能需要使用usize实现
        var i = index;
        if (i < 0) i += @intCast(count);
        if (i >= 0 and i < count) return @intCast(i);
        return null;
    }

    pub inline fn validateIndex(self: *@This(), arg: Value, count: usize, name: []const u8) ?u32 {
        // TODO 可能需要使用usize实现
        if (!self.validateNum(arg, name)) return null;
        return self.validateIndexValue(count, arg.asNum(), name);
    }

    pub inline fn validateFunc(self: *@This(), arg: Value, name: []const u8) bool {
        if (arg.isClosure()) return true;
        self.fiber.?.err = self.stringFormat("$ must be a function.", .{name});
        return false;
    }

    pub inline fn validateNum(self: *@This(), arg: Value, name: []const u8) bool {
        if (arg.isNum()) return true;
        self.fiber.?.err = self.stringFormat("$ must be a number.", .{name});
        return false;
    }

    pub inline fn validateIntValue(self: *@This(), arg: f64, name: []const u8) bool {
        if (@trunc(arg) == arg) return true;
        self.fiber.?.err = self.stringFormat("$ must be an integer.", .{name});
        return false;
    }

    pub inline fn validateInt(self: *@This(), arg: Value, name: []const u8) bool {
        if (!self.validateNum(arg, name)) return false;
        return self.validateIntValue(arg.asNum(), name);
    }

    pub inline fn validateKey(self: *@This(), arg: Value) bool {
        if (arg.isValidKey()) return true;
        self.fiber.?.err = self.newString("Key must be a value type.");
        return false;
    }

    pub inline fn validateString(self: *@This(), arg: Value, name: []const u8) bool {
        if (arg.isString()) return true;
        self.fiber.?.err = self.stringFormat("$ must be a string.", .{name});
        return false;
    }

    pub fn calculateRange(self: *@This(), range: *ObjRange, len: *u32, step: *i32) ?u32 {
        // TODO 改进 len 类型
        step.* = 0;

        // 边界情况: 空range
        //     即便空列表, 也可使用list[0..-1] 和 list[0..list.count] 拷贝整个列表
        const len_f64: f64 = @floatFromInt(len.*);
        const targetTo: f64 = if (range.isInclusive) -1 else len_f64;
        if (range.from == len_f64 and range.to == targetTo) {
            len.* = 0;
            return 0;
        }

        const from = self.validateIndexValue(len.*, range.from, "Range start") orelse return null;

        // 手动检查边界, 以处理开区间
        if (!self.validateIntValue(range.to, "Range end")) return null;

        const from_f64: f64 = @floatFromInt(from);
        var to = range.to;
        // 负索引: 从末尾开始
        if (to < 0) to += @floatFromInt(len.*);
        // 将开区间转换为闭区间
        if (!range.isInclusive) {
            // 当开区间的start和end相同时, 代表空range
            if (to == from_f64) {
                len.* = 0;
                return from;
            }

            // 移动端点以使其包含在内, (同时处理递增和递减区间)
            if (to >= from_f64) to -= 1 else to += 1;
        }
        // 边界检查
        if (to < 0 or to >= @as(f64, @floatFromInt(len.*))) {
            self.fiber.?.err = self.newString("Range end out of bounds.");
            return null;
        }
        // TODO 不是很简洁
        len.* = @as(u32, @intCast(@abs(@as(isize, @intFromFloat(from_f64 - to))) + 1));
        step.* = if (from_f64 <= to) 1 else -1;
        return from;
    }

    pub fn symbolTableAdd(self: *@This(), symbols: *SymbolTable, name: []const u8) usize {
        const nameValue = self.newString(name);
        const symbol = nameValue.asString();
        self.pushRoot(&symbol.obj);
        symbols.push(symbol);
        self.popRoot();
        return symbols.count - 1;
    }

    pub fn symbolTableEnsure(self: *@This(), symbols: *SymbolTable, name: []const u8) usize {
        return self.symbolTableFind(symbols, name) orelse self.symbolTableAdd(symbols, name);
    }

    pub fn pushRoot(self: *@This(), obj: *Obj) void {
        Utils.assert(self.numTempRoots < Constants.C.MAX_TEMP_ROOTS, "Too many temporary roots.");
        self.tempRoots[self.numTempRoots] = obj;
        self.numTempRoots += 1;
    }

    pub fn popRoot(self: *@This()) void {
        Utils.assert(self.numTempRoots > 0, "No temporary roots to release.");
        self.numTempRoots -= 1;
    }

    pub fn symbolTableFind(_: *@This(), symbols: *SymbolTable, name: []const u8) ?usize {
        for (0..symbols.count) |i| {
            const symbol = symbols.at(i);
            // TODO 这个比较代码是否正确? 考虑两者长度不同的情况
            if (std.mem.eql(u8, name, symbol.value)) return i;
        }
        return null;
    }

    pub fn randomBindForeignClass(self: *@This(), module: []const u8, className: []const u8) ZrenForeignClassMethods {
        _ = .{ self, module };
        Utils.assert(std.mem.eql(u8, className, "Random"), "Should be in Random class.");
        return .{ .allocate = randomAllocate };
    }

    pub fn captureUpvalue(self: *@This(), fiber: *ObjFiber, local: *Value, localStackIndex: usize) *ObjUpvalue {
        if (fiber.openUpvalues == null) {
            fiber.openUpvalues = self.newUpvalue(local, localStackIndex);
            return fiber.openUpvalues.?;
        }
        var preUpvalue: ?*ObjUpvalue = null;
        var upvalue = fiber.openUpvalues;
        // 遍历到栈底, 直到找到先前存在的upvalue或传递到它应该在的位置
        while (upvalue) |uv| : (upvalue = uv.next) {
            if (@intFromPtr(uv.value) <= @intFromPtr(local)) break; // TODO 这里有问题
            preUpvalue = uv;
        }

        // 为当前local找到已存在的upvalue
        if (upvalue != null and upvalue.?.value == local) return upvalue.?;

        // 这里已经是栈底了, 所以local一定没有upvalue.
        // 创建一个新的upvalue并插入到合适的位置, 以保持列表有序
        const createdUpvalue = self.newUpvalue(local, localStackIndex);
        if (preUpvalue) |prev| {
            prev.next = createdUpvalue;
        } else {
            // 新的upvalue是列表中的第一个
            fiber.openUpvalues = createdUpvalue;
        }
        createdUpvalue.next = upvalue;
        return createdUpvalue;
    }

    fn findForeignMethod(self: *@This(), moduleName: []const u8, className: []const u8, isStatic: bool, signature: []const u8) ?ZrenForeignMethodFn {
        var method: ?ZrenForeignMethodFn = null;

        if (self.config.bindForeignMethodFn) |bfn| {
            method = bfn(self, moduleName, className, isStatic, signature);
        }

        // 如果host没有提供, 检查是否是可选的
        if (method == null) {
            if (std.mem.eql(u8, moduleName, "meta")) {
                method = self.metaBindForeignMethod(className, isStatic, signature);
            }
            if (std.mem.eql(u8, moduleName, "random")) {
                method = self.randomBindForeignMethod(className, isStatic, signature);
            }
        }

        return method;
    }

    fn metaBindForeignMethod(self: *@This(), className: []const u8, isStatic: bool, signature: []const u8) ?ZrenForeignMethodFn {
        _ = self;
        // 在meta模块中只有一个外部方法
        // TODO 这些assert 检查及相应的erro错误信息
        Utils.assert(std.mem.eql(u8, className, "Meta"), "Should be in Meta class.");
        Utils.assert(isStatic, "Should be static.");

        if (std.mem.eql(u8, signature, "compile_(_,_,_)")) {
            return metaCompile;
        }

        if (std.mem.eql(u8, signature, "getModuleVariables_(_)")) {
            return metaGetModuleVariables;
        }
        return null;
    }

    fn randomBindForeignMethod(self: *@This(), className: []const u8, isStatic: bool, signature: []const u8) ?ZrenForeignMethodFn {
        _ = .{ self, isStatic };
        Utils.assert(std.mem.eql(u8, className, "Random"), "Should be in Random class.");
        if (std.mem.eql(u8, signature, "<allocate>")) return randomAllocate;
        if (std.mem.eql(u8, signature, "seed_()")) return randomSeed0;
        if (std.mem.eql(u8, signature, "seed_(_)")) return randomSeed1;
        if (std.mem.eql(u8, signature, "seed_(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)")) return randomSeed16;
        if (std.mem.eql(u8, signature, "float()")) return randomFloat;
        if (std.mem.eql(u8, signature, "int()")) return randomInt0;
        return null;
    }

    fn metaCompile(vm: *@This()) void {
        const source: []const u8 = vm.getSlotString(1);
        const isExpression: bool = vm.getSlotBool(2);
        const printErrors: bool = vm.getSlotBool(3);

        // TODO Allow passing in module?
        // 查找调用点周围的模块.
        // -2 会沿着调用栈向上移动，假设meta模块在到达用户代码之前有一个间接层级.
        // 对meta的任意修改都可能需要调整这个常量.
        const currentFiber = vm.fiber orelse return;

        // TODO 对齐c++实现
        const func = currentFiber.frames[currentFiber.numFrames - 2].closure.func;
        const module = func.module.?.name;
        const closure = vm.compileSource(module.?.value, source, isExpression, printErrors);
        vm.apiStackAt(0).* = if (closure) |c| c.asObj().toVal() else .NULL_VAL;
    }

    fn metaGetModuleVariables(vm: *@This()) void {
        vm.ensureSlots(3);

        const moduleValue = vm.modules.mapGet(vm.apiStackAt(1).*);
        if (moduleValue.isUndefined()) {
            vm.apiStackAt(0).* = .NULL_VAL;
            return;
        }

        const module = moduleValue.asModule();
        const names = vm.newList(module.variableNames.count);
        vm.apiStackAt(0).* = names.asObj().toVal();

        // 将元素初始化为null, 以便在下面分配字符串时发生垃圾回收.
        for (0..names.elements.count) |i| {
            names.elements.rat(i).* = .NULL_VAL;
        }

        for (0..names.elements.count) |i| {
            names.elements.rat(i).* = module.variableNames.at(i).asObj().toVal();
        }
    }

    pub fn callForeign(self: *@This(), forein: ZrenForeignMethodFn, num_args: usize) void {
        Utils.assert(self.isApiStackNull(), "Cannot already be in foreign call.");
        const fiber: *ObjFiber = self.fiber orelse return;
        self.setApiStack(fiber.stack.buffer, fiber.stack.top - num_args);
        forein(self);
        fiber.loadStack(self.apiStackOffset + 1); // 丢弃临时变量和参数, 但保留一个槽位用于返回值.
        self.clearApiStack();
    }

    pub fn callFunction(self: *@This(), closure: *ObjClosure, num_args: usize) void {
        const fiber = self.fiber orelse return;
        // TODO 这里需要改进  maxSlots 可能为负(原版实现)
        const needed = fiber.stack.top +% closure.func.maxSlots;
        self.ensureStack(fiber, needed);
        fiber.pushFrame(closure, fiber.stack.top - num_args);
    }

    pub fn ensureStack(self: *@This(), fiber: *ObjFiber, needed: usize) void {
        if (fiber.stack.buffer.len >= needed) return;
        const capacity = Utils.powerOf2Ceil(needed);

        const oldStack = fiber.stack.buffer.ptr;
        fiber.stack.resize(capacity) catch unreachable;
        if (@intFromPtr(&fiber.stack.buffer.ptr) == @intFromPtr(oldStack)) return;
        self.setApiStackOnly(fiber.stack.buffer[0..]);
        fiber.remapFrames();
        fiber.remapUpvalues();
    }

    pub inline fn isApiStackNull(self: *const @This()) bool {
        return self.apiStack.len == 0;
    }

    pub inline fn clearApiStack(self: *@This()) void {
        self.apiStack = &.{};
        self.apiStackOffset = 0;
    }

    pub inline fn setApiStackOnly(self: *@This(), stack: []Value) void {
        self.apiStack = if (self.apiStack.len != 0) stack else &.{}; // TODO 检查这里的正确性
    }

    pub fn setApiStack(self: *@This(), stack: []Value, offset: usize) void {
        self.apiStack = stack;
        self.apiStackOffset = offset;
    }

    pub fn apiStackAt(self: *@This(), index: usize) *Value {
        return &self.apiStack[self.apiStackOffset + index];
    }

    pub inline fn completeCall(self: *@This(), numArgs: u8, symbol: u16, args: []Value, classObj: *ObjClass) bool {
        const m = classObj.methods.rat(symbol);
        if (symbol >= classObj.methods.count or m.method_type == .METHOD_NONE) {
            self.methodNotFound(classObj, symbol);
            self.throwErr();
            return false;
        }

        switch (m.method_type) {
            .METHOD_PRIMITIVE => {
                if (m.as.primitive(self, args)) {
                    self.fiber.?.stack.nOffset(numArgs - 1); // 返回结果在第一个参数位置, 丢弃其他栈槽
                } else {
                    const fiber = self.fiber orelse return true;
                    if (fiber.hasErr()) {
                        self.throwErr();
                    } else {
                        fiber.loadFrame();
                    }
                }
            },
            .METHOD_FUNCTION_CALL => {
                const fiber = self.fiber orelse return false;
                if (args[0].asClosure().func.checkArity(numArgs)) {
                    _ = m.as.primitive(self, args);
                    fiber.loadFrame();
                } else {
                    const msg: []const u8 = "Function expects more arguments.";
                    fiber.err = self.newString(msg);
                }
                if (fiber.hasErr()) self.throwErr();
            },
            .METHOD_FOREIGN => {
                self.callForeign(m.as.foreign.?, numArgs);
                if (self.fiber.?.hasErr()) self.throwErr();
            },
            .METHOD_BLOCK => {
                self.callFunction(m.as.closure, numArgs);
                self.fiber.?.loadFrame();
            },
            .METHOD_NONE => unreachable,
        }
        return false;
    }

    pub fn throwErr(self: *@This()) void {
        var cfiber: ?*ObjFiber = self.fiber;
        const err = self.fiber.?.err;
        while (cfiber) |f| {
            f.err = err; // 每个调用链中的fiber都因相同的错误中止.

            // 如果调用方是以`try`的方式运行此fiber, 则将错误传递给它并停止.
            if (f.state == .FIBER_TRY) {
                f.caller.?.peek(1).* = err; // 将错误传递给调用方.
                self.fiber = f.caller;
                self.fiber.?.loadFrame();
                return;
            }
            cfiber = f.caller;
            f.caller = null;
        }
        // 如果我们到达这里, 代表没有fiber捕获错误, 所以显示堆栈跟踪.
        self.debugPrintStackTrace();
        self.fiber = null;
        self.clearApiStack();
    }

    pub fn debugPrintStackTrace(self: *@This()) void {
        const errorFn = self.config.errorFn orelse return;
        const fiber = self.fiber orelse return;
        if (fiber.err.isString()) {
            errorFn(self, .ERROR_RUNTIME, &.{}, null, fiber.err.asString().value);
        } else {
            errorFn(self, .ERROR_RUNTIME, &.{}, null, "[error object]");
        }
        // 反向遍历调用栈.
        var i = fiber.numFrames;
        while (i > 0) {
            i -= 1;
            const frame: *CallFrame = &fiber.frames[i];
            const func: *ObjFunc = frame.closure.func;
            const module = func.module orelse continue;
            const module_name = module.name orelse continue;
            const line = func.debug.source_lines.at(frame.ipOffset - 1);
            errorFn(self, .ERROR_STACK_TRACE, module_name.value, line, func.debug.name);
        }
    }

    pub fn debugTraceInstructions(self: *@This()) void {
        comptime if (builtin.mode != .Debug) return;
        self.fiber.?.dumpStack();
        const func = self.fiber.?.frame.closure.func;
        // TODO 这里计算偏移有问题
        // const i = (@intFromPtr(self.fiber.?.frame.ip) - @intFromPtr(func.code.data.items.ptr)) / @sizeOf(u8);
        _ = self.dumpInstruction(func, self.fiber.?.frame.ipOffset, null);
    }

    pub fn dumpInstruction(self: *@This(), func: *ObjFunc, i: usize, last_line: ?*usize) isize {
        const start = i;
        var iterWrap = func.code.iterWrap(i);
        const code: OpCode = @enumFromInt(iterWrap.rbyte());

        const line = func.debug.source_lines.at(i);
        if (last_line == null or line != last_line.?.*) {
            print("{d:>4}:", .{line});
            if (last_line != null) last_line.?.* = line;
        } else {
            print("     ", .{});
        }

        print(" {d:0>4} ", .{i});

        switch (code) {
            .CODE_CONSTANT => {
                const constant = iterWrap.rshort();
                print("{s} {d:>5} '", .{ @tagName(code), constant });
                func.constants.rat(constant).dumpVal();
                print("'\n", .{});
            },
            .CODE_NULL,
            .CODE_FALSE,
            .CODE_TRUE,
            .CODE_LOAD_LOCAL_0,
            .CODE_LOAD_LOCAL_1,
            .CODE_LOAD_LOCAL_2,
            .CODE_LOAD_LOCAL_3,
            .CODE_LOAD_LOCAL_4,
            .CODE_LOAD_LOCAL_5,
            .CODE_LOAD_LOCAL_6,
            .CODE_LOAD_LOCAL_7,
            .CODE_LOAD_LOCAL_8,
            => print("{s}\n", .{@tagName(code)}),

            .CODE_LOAD_LOCAL,
            .CODE_STORE_LOCAL,
            .CODE_LOAD_UPVALUE,
            .CODE_STORE_UPVALUE,
            => print("{s} {d:>5}\n", .{ @tagName(code), iterWrap.rbyte() }),

            .CODE_LOAD_MODULE_VAR,
            .CODE_STORE_MODULE_VAR,
            => {
                const slot = iterWrap.rshort();
                print("{s} {d:>5} '{s}'\n", .{ @tagName(code), slot, func.module.?.variableNames.at(slot).value });
            },

            .CODE_LOAD_FIELD_THIS,
            .CODE_STORE_FIELD_THIS,
            .CODE_LOAD_FIELD,
            .CODE_STORE_FIELD,
            => print("{s} {d:>5}\n", .{ @tagName(code), iterWrap.rbyte() }),

            .CODE_POP => print("{s}\n", .{@tagName(code)}),

            .CODE_CALL_0,
            .CODE_CALL_1,
            .CODE_CALL_2,
            .CODE_CALL_3,
            .CODE_CALL_4,
            .CODE_CALL_5,
            .CODE_CALL_6,
            .CODE_CALL_7,
            .CODE_CALL_8,
            .CODE_CALL_9,
            .CODE_CALL_10,
            .CODE_CALL_11,
            .CODE_CALL_12,
            .CODE_CALL_13,
            .CODE_CALL_14,
            .CODE_CALL_15,
            .CODE_CALL_16,
            => {
                const symbol = iterWrap.rshort();
                print("{s} {d:>5} '{s}'\n", .{ @tagName(code), symbol, self.methodNames.at(symbol).value });
            },

            .CODE_SUPER_0,
            .CODE_SUPER_1,
            .CODE_SUPER_2,
            .CODE_SUPER_3,
            .CODE_SUPER_4,
            .CODE_SUPER_5,
            .CODE_SUPER_6,
            .CODE_SUPER_7,
            .CODE_SUPER_8,
            .CODE_SUPER_9,
            .CODE_SUPER_10,
            .CODE_SUPER_11,
            .CODE_SUPER_12,
            .CODE_SUPER_13,
            .CODE_SUPER_14,
            .CODE_SUPER_15,
            .CODE_SUPER_16,
            => {
                const symbol = iterWrap.rshort();
                const superclass = iterWrap.rshort();
                print("{s} {d:>5} '{s}' {d:>5}\n", .{ @tagName(code), symbol, self.methodNames.at(symbol).value, superclass });
            },

            .CODE_JUMP,
            .CODE_JUMP_IF,
            .CODE_AND,
            .CODE_OR,
            => {
                const offset = iterWrap.rshort();
                print("{s} {d:>5} to {d}\n", .{ @tagName(code), offset, iterWrap.pos + offset });
            },

            .CODE_LOOP => {
                const offset = iterWrap.rshort();
                print("{s} {d:>5} to {d}\n", .{ @tagName(code), offset, iterWrap.pos - offset });
            },

            .CODE_CLOSE_UPVALUE,
            .CODE_RETURN,
            => print("{s}\n", .{@tagName(code)}),

            .CODE_CLOSURE => {
                const constant = iterWrap.rshort();
                print("{s} {d:>5} ", .{ @tagName(code), constant });
                func.constants.rat(constant).dumpVal();
                print(" ", .{});
                const loaded_func = func.constants.rat(constant).asObj().asFunc();
                for (0..loaded_func.numUpvalues) |j| {
                    const isLocal = iterWrap.rbyte();
                    const index = iterWrap.rbyte();
                    if (j > 0) print(", ", .{});
                    print("{s} {d}", .{ if (isLocal != 0) "local" else "upvalue", index });
                }
                print("\n", .{});
            },

            .CODE_CONSTRUCT,
            .CODE_FOREIGN_CONSTRUCT,
            => print("{s}\n", .{@tagName(code)}),

            .CODE_CLASS => {
                const numFields = iterWrap.rbyte();
                print("{s} {d:>5}\n", .{ @tagName(code), numFields });
            },

            .CODE_FOREIGN_CLASS,
            .CODE_END_CLASS,
            => print("{s}\n", .{@tagName(code)}),

            .CODE_METHOD_INSTANCE,
            .CODE_METHOD_STATIC,
            => {
                const symbol = iterWrap.rshort();
                print("{s} {d:>5} '{s}'\n", .{ @tagName(code), symbol, self.methodNames.at(symbol).value });
            },

            .CODE_END_MODULE => print("{s}\n", .{@tagName(code)}),

            .CODE_IMPORT_MODULE,
            .CODE_IMPORT_VARIABLE,
            => {
                const name = iterWrap.rshort();
                print("{s} {} '", .{ @tagName(code), name });
                func.constants.rat(name).dumpVal();
                print("'\n", .{});
            },

            .CODE_END => print("{s}\n", .{@tagName(code)}),
        }

        if (code == .CODE_END) return -1;
        return @intCast(iterWrap.pos - start);
    }

    fn initializeCore(self: *@This()) void {
        const coreModule = self.newModule(null);
        self.pushRoot(coreModule.asObj());
        // 核心模块的key在模块map中为null
        self.mapSet(self.modules, .NULL_VAL, coreModule.asObj().toVal());
        self.popRoot();

        // 定义root Object类. 这个类没有父类, 所以需要特殊处理.
        ObjectModule._install(self, coreModule);

        // 现在, 我们可以定义Class, 它是Object的子类.
        ClassModule._install(self, coreModule);

        // 最后, 定义 Object 的元类, 它是 Class 的子类.
        MetaClassModule._install(self, coreModule);
        // 剩余的类, 可以正常定义.
        _ = self.interpret(&.{}, Constants.coreModuleSource);

        BoolModule._install(self, coreModule);

        FiberModule._install(self, coreModule);

        FuncModule._install(self, coreModule);

        NullModule._install(self, coreModule);

        NumModule._install(self, coreModule);

        StringModule._install(self, coreModule);

        ListModule._install(self, coreModule);

        MapModule._install(self, coreModule);

        RangeModule._install(self, coreModule);

        SystemModule._install(self, coreModule);

        // 当引导核心类型并运行核心模块时，已经创建了大量的字符串对象，其中许多对象在 stringClass 存储在 VM 之前就已经实例化.
        // 其中一些 *必须* 首先创建——字符串本身的 ObjClass 对象有一个指向其名称的 ObjString 对象的引用.
        // 这些对象目前都有一个 NULL 类对象指针，所以现在返回并分配它们，因为字符串类是已知的.
        var obj = self.first;
        while (obj) |o| : (obj = o.next) {
            if (o.isObjType(.OBJ_STRING)) o.class_obj = self.stringClass;
        }
    }

    pub fn releaseHandle(self: *@This(), handle: ?*ZrenHandle) void {
        Utils.assert(handle != null, "Handle cnanot be NULL.");

        // 如果我们要释放第一个句柄, 则更新 VM 的头指针
        if (self.handles == handle) self.handles = handle.?.next;

        // 从链表中移除
        if (handle.?.prev) |hp| hp.next = handle.?.next;
        if (handle.?.next) |hn| hn.prev = handle.?.prev;

        // 清理. 这并不是严格必要的，因为我们将会释放它，但是它使得调试更容易。
        handle.?.prev = null;
        handle.?.next = null;
        handle.?.value = .NULL_VAL;
        self.allocator.destroy(handle.?); // TODO 检查这里的逻辑
    }

    pub fn interpret(self: *@This(), module: []const u8, source: []const u8) ZrenInterpretResult {
        var closure = self.compileSource(module, source, false, true) orelse return .RESULT_COMPILE_ERROR;

        self.pushRoot(closure.asObj());
        const fiber = self.newFiber(closure);
        self.popRoot();
        self.clearApiStack();
        return self.runInterpreter(fiber);
    }

    pub fn runFiber(self: *@This(), fiber: *ObjFiber, args: []Value, isCall: bool, hasValue: bool, comptime verb: []const u8) bool {
        if (fiber.hasErr()) {
            self.fiber.?.err = self.stringFormat("Cannot $ an aborted fiber.", .{verb});
            return false;
        }

        if (isCall) {
            // 无法调用已经调用的fiber, 但可以直接transfer到它, 所以这里要检查在`isCall`上运行.
            // 这样, 在恢复一个挂起的fiber后, 它将运行并返回到调用它的fiber, 以此类推.

            if (fiber.caller != null) {
                self.fiber.?.err = self.newString("Fiber has already been called.");
                return false;
            }

            if (fiber.state == .FIBER_ROOT) {
                self.fiber.?.err = self.newString("Cannot call root fiber.");
                return false;
            }

            fiber.caller = self.fiber; // 记录调用者
        }

        if (fiber.numFrames == 0) {
            self.fiber.?.err = self.stringFormat("Cannot $ a finished fiber.", .{verb});
            return false;
        }

        // 当正在调用的fiber恢复时，将把调用的结果存储在其堆栈中.
        // 如果调用有两个参数（fiber和值），我们只需要一个用于结果的槽位，所以现在丢弃另一个槽位.
        if (hasValue) self.fiber.?.stack.nOffset(1);

        if (fiber.numFrames == 1 and fiber.frames[0].ipOffset == 0) {
            // 当前fiber第一次被调用, 绑定参数.
            if (fiber.frames[0].closure.func.arity == 1) {
                fiber.push(if (hasValue) args[1] else .NULL_VAL);
            }
        } else {
            fiber.peek(1).* = if (hasValue) args[1] else .NULL_VAL;
        }

        self.fiber = fiber;
        return false;
    }

    pub fn runInterpreter(self: *@This(), ifiber: *ObjFiber) ZrenInterpretResult {
        ifiber.state = .FIBER_ROOT;
        ifiber.loadFrame();
        // if (builtin.mode == .Debug) {
        //     // TODO 用于测试
        //     const colNum: usize = 20;
        //     const rowNum: usize = 20;
        //     for (0..colNum) |i| {
        //         for (0..rowNum) |j| {
        //             if (i * rowNum + j >= ifiber.frame.closure.func.code.count) break;
        //             if ((i * rowNum + j) % 2 == 1) continue;
        //             print("{d:03} ", .{ifiber.frame.closure.func.code.at(i * rowNum + j)});
        //         }
        //         print("\n", .{});
        //     }
        //     print("\n------------------------------------------------------------\n", .{});
        // }

        self.fiber = ifiber;

        // ----------------call阶段使用的----------------
        var numArgs: u8 = 0;
        var symbol: u16 = 0;
        var args: []Value = undefined;
        var classObj: *ObjClass = undefined;
        // ----------------------------------------------
        const CODE_LOAD_LOCAL_0 = OpCode.CODE_LOAD_LOCAL_0.num();
        const CODE_CALL_0 = OpCode.CODE_CALL_0.num();
        const CODE_SUPER_0 = OpCode.CODE_SUPER_0.num();
        var inst: u8 = undefined;
        var count: u32 = 1;
        count = count;
        while (self.fiber) |fiber| {
            // if (count < 100000) {
            //     print("{d} ======\n", .{count});
            //     self.debugTraceInstructions();
            // }
            // count += 1;
            // self.debugTraceInstructions();
            inst = fiber.frame.rbyte();
            const op: OpCode = @enumFromInt(inst);
            switch (op) {
                .CODE_LOAD_LOCAL_0,
                .CODE_LOAD_LOCAL_1,
                .CODE_LOAD_LOCAL_2,
                .CODE_LOAD_LOCAL_3,
                .CODE_LOAD_LOCAL_4,
                .CODE_LOAD_LOCAL_5,
                .CODE_LOAD_LOCAL_6,
                .CODE_LOAD_LOCAL_7,
                .CODE_LOAD_LOCAL_8,
                => fiber.push(fiber.frame.stackAt(inst - CODE_LOAD_LOCAL_0).*),

                .CODE_LOAD_LOCAL,
                => fiber.push(fiber.frame.stackAt(fiber.frame.rbyte()).*),

                .CODE_LOAD_FIELD_THIS => {
                    const field = fiber.frame.rbyte();
                    const receiver = fiber.frame.stackAt(0);
                    Utils.assert(receiver.isInstance(), "Receiver should be instance.");
                    const instance = receiver.asInstance();
                    Utils.assert(field < instance.asObj().class_obj.num_fields.?, "Out of bounds field.");
                    fiber.push(instance.fields[field]);
                },

                .CODE_POP => fiber.drop(),
                .CODE_NULL => fiber.push(.NULL_VAL),
                .CODE_FALSE => fiber.push(.FALSE_VAL),
                .CODE_TRUE => fiber.push(.TRUE_VAL),

                .CODE_STORE_LOCAL,
                => fiber.frame.stackAt(fiber.frame.rbyte()).* = fiber.peek(1).*,

                .CODE_CONSTANT,
                => fiber.push(fiber.frame.readConstant(fiber.frame.rshort()).*),

                .CODE_CALL_0,
                .CODE_CALL_1,
                .CODE_CALL_2,
                .CODE_CALL_3,
                .CODE_CALL_4,
                .CODE_CALL_5,
                .CODE_CALL_6,
                .CODE_CALL_7,
                .CODE_CALL_8,
                .CODE_CALL_9,
                .CODE_CALL_10,
                .CODE_CALL_11,
                .CODE_CALL_12,
                .CODE_CALL_13,
                .CODE_CALL_14,
                .CODE_CALL_15,
                .CODE_CALL_16,
                => {
                    numArgs = inst - CODE_CALL_0 + 1;
                    symbol = fiber.frame.rshort();
                    args = fiber.stack.topNegSlice(numArgs);
                    classObj = self.getClassInline(args[0]).?;
                    if (self.completeCall(numArgs, symbol, args, classObj)) {
                        return .RESULT_SUCCESS;
                    }
                },

                .CODE_SUPER_0,
                .CODE_SUPER_1,
                .CODE_SUPER_2,
                .CODE_SUPER_3,
                .CODE_SUPER_4,
                .CODE_SUPER_5,
                .CODE_SUPER_6,
                .CODE_SUPER_7,
                .CODE_SUPER_8,
                .CODE_SUPER_9,
                .CODE_SUPER_10,
                .CODE_SUPER_11,
                .CODE_SUPER_12,
                .CODE_SUPER_13,
                .CODE_SUPER_14,
                .CODE_SUPER_15,
                .CODE_SUPER_16,
                => {
                    numArgs = inst - CODE_SUPER_0 + 1;
                    symbol = fiber.frame.rshort();
                    args = fiber.stack.topNegSlice(numArgs);
                    classObj = fiber.frame.readConstant(fiber.frame.rshort()).asClass();
                    if (self.completeCall(numArgs, symbol, args, classObj)) {
                        return .RESULT_SUCCESS;
                    }
                },

                .CODE_LOAD_UPVALUE => fiber.push(fiber.frame.readUpvalue(fiber.frame.rbyte()).*),
                .CODE_STORE_UPVALUE => fiber.frame.writeUpvalue(fiber.frame.rbyte(), fiber.peek(1)),

                .CODE_LOAD_MODULE_VAR => fiber.push(fiber.frame.readModuleVar(fiber.frame.rshort()).*),
                .CODE_STORE_MODULE_VAR => fiber.frame.writeModuleVar(fiber.frame.rshort(), fiber.peek(1)),

                .CODE_STORE_FIELD_THIS => {
                    const field = fiber.frame.rbyte();
                    var receiver = fiber.frame.stackAt(0);
                    Utils.assert(receiver.isInstance(), "Receiver should be instance.");
                    const instance = receiver.asInstance();
                    Utils.assert(field < instance.asObj().class_obj.num_fields.?, "Out of bounds field.");
                    instance.fields[field] = fiber.peek(1).*;
                },

                .CODE_LOAD_FIELD => {
                    const field = fiber.frame.rbyte();
                    var receiver = fiber.pop();
                    Utils.assert(receiver.isInstance(), "Receiver should be instance.");
                    const instance = receiver.asInstance();
                    Utils.assert(field < instance.asObj().class_obj.num_fields.?, "Out of bounds field.");
                    fiber.push(instance.fields[field]);
                },

                .CODE_STORE_FIELD => {
                    const field = fiber.frame.rbyte();
                    var receiver = fiber.pop();
                    Utils.assert(receiver.isInstance(), "Receiver should be instance.");
                    const instance = receiver.asInstance();
                    Utils.assert(field < instance.asObj().class_obj.num_fields.?, "Out of bounds field.");
                    instance.fields[field] = fiber.peek1();
                },

                .CODE_JUMP => fiber.frame.ipPosOffset(fiber.frame.rshort()),
                .CODE_LOOP => fiber.frame.ipNegOffset(fiber.frame.rshort()),
                .CODE_JUMP_IF => {
                    const off = fiber.frame.rshort();
                    if (fiber.pop().isFalse()) fiber.frame.ipPosOffset(off);
                },

                .CODE_AND => {
                    const off = fiber.frame.rshort();
                    if (fiber.peek(1).isFalse()) fiber.frame.ipPosOffset(off) else fiber.drop();
                },

                .CODE_OR => {
                    const off = fiber.frame.rshort();
                    if (!fiber.peek(1).isFalse()) fiber.frame.ipPosOffset(off) else fiber.drop();
                },

                .CODE_CLOSE_UPVALUE => fiber.closeUpvalues(fiber.pop()), // 如果有局部upvalue, 将其封闭,

                .CODE_RETURN => {
                    const result = fiber.pop();
                    fiber.dropFrame();
                    fiber.closeUpvalues(fiber.frame.stackAt(0));
                    // 如果当前fiber已经执行完毕, 将其结束.
                    if (fiber.numFrames == 0) {
                        // 如果有caller: 代表是函数调用 -> 返回到caller
                        if (fiber.caller) |caller| {
                            const resumingFiber = caller;
                            resumingFiber.stack.peek(1).* = result.*; // 存储结果到fiber栈顶下一个位置
                            fiber.caller = null; // 将当前fiber的caller置空
                            self.fiber = resumingFiber; // 切换到调用者
                        } else { // 如果没有caller: 代表是main -> 执行完成
                            fiber.stack.write(0, result); // 将终值存储在第一个槽, 以便C API获取.
                            fiber.loadStack(1);
                            return .RESULT_SUCCESS;
                        }
                    } else {
                        fiber.frame.writeStack(0, result); // 将结果存储在栈底, 以便调用者可以获取.
                        fiber.loadStack(fiber.frame.stackOffset + 1); // 留一个栈槽存储结果, 其他丢弃
                    }
                    self.fiber.?.loadFrame();
                },

                .CODE_CONSTRUCT => {
                    Utils.assert(fiber.frame.stackAt(0).isClass(), "'this' should be a class.");
                    fiber.frame.writeStack(0, &self.newInstance(fiber.frame.stackAt(0).asClass()));
                },

                .CODE_FOREIGN_CONSTRUCT => {
                    Utils.assert(fiber.frame.stackAt(0).isClass(), "'this' should be a class.");
                    self.createForeign(fiber, fiber.frame.stackAt(0).asClass());
                    if (fiber.hasErr()) self.throwErr();
                },

                .CODE_CLOSURE => {
                    const function = fiber.frame.readConstant(fiber.frame.rshort()).asFunc();
                    const closure = self.newClosure(function);
                    fiber.push(closure.asObj().toVal());

                    // 捕获upvalue
                    for (0..function.numUpvalues) |i| {
                        const isLocal = fiber.frame.rbyte();
                        const index = fiber.frame.rbyte();
                        if (isLocal != 0) { // 局部变量, 构造一个新 upvalue 来封闭父级的局部变量.
                            const localStackIndex: usize = fiber.frame.stackOffset + index;
                            closure.upvalues[i] = self.captureUpvalue(fiber, fiber.frame.stackAt(index), localStackIndex);
                        } else { // 非局部变量, 使用当前调用帧的同一个 upvalue.
                            closure.upvalues[i] = fiber.frame.closure.upvalues[index];
                        }
                    }
                },

                .CODE_CLASS => {
                    self.createClass(fiber.frame.rbyte(), null);
                    if (fiber.hasErr()) self.throwErr();
                },

                .CODE_FOREIGN_CLASS => {
                    self.createClass(null, fiber.frame.closure.func.module);
                    if (fiber.hasErr()) self.throwErr();
                },

                .CODE_END_CLASS => {
                    self.endClass();
                    if (fiber.hasErr()) self.throwErr();
                },

                .CODE_METHOD_INSTANCE,
                .CODE_METHOD_STATIC,
                => {
                    symbol = fiber.frame.rshort();
                    classObj = fiber.peek(1).asClass();
                    self.gBindMethod(op, symbol, fiber.frame.closure.func.module.?, classObj, fiber.peek(2).*);
                    if (fiber.hasErr()) {
                        self.throwErr();
                        break;
                    }
                    fiber.drop();
                    fiber.drop();
                },

                .CODE_END_MODULE => {
                    self.lastModule = fiber.frame.closure.func.module;
                    fiber.push(.NULL_VAL);
                },

                .CODE_IMPORT_MODULE => {
                    fiber.push(self.importModule(fiber.frame.readConstant(fiber.frame.rshort())));
                    if (fiber.hasErr()) {
                        self.throwErr();
                        break;
                    }

                    const result = fiber.peek(1);

                    if (result.isClosure()) { // 如果结果是闭包, 则调用它来执行模块体.
                        const closure = result.asClosure();
                        self.callFunction(closure, 1);
                        fiber.loadFrame();
                    } else { // 否则, 证明模块已导入, 将其存储在lastModule, 以便后续导入变量
                        self.lastModule = result.asModule();
                    }
                },

                .CODE_IMPORT_VARIABLE => {
                    const variable = fiber.frame.readConstant(fiber.frame.rshort());
                    Utils.assert(self.lastModule != null, "Should have already imported module.");
                    const result = self.getModuleVar(self.lastModule.?, variable);
                    if (fiber.hasErr()) self.throwErr() else fiber.push(result);
                },
                .CODE_END => unreachable,
            }
        }
        return .RESULT_RUNTIME_ERROR;
    }

    pub fn makeCallHandle(vm: *@This(), signature: []const u8) *ZrenHandle {
        var numParams: u8 = 0;
        if (signature[signature.len - 1] == ')') {
            var i = signature.len;
            while (i > 0) {
                i -= 1;
                if (signature[i] == '(') break;
                if (signature[i] == '_') numParams += 1;
            }
        }

        if (signature[0] == '[') {
            var i: usize = 0;
            while (i < signature.len and signature[i] != ']') : (i += 1) {
                if (signature[i] == '_') numParams += 1;
            }
        }

        // 添加signature到方法表.
        const method = vm.symbolTableEnsure(&vm.methodNames, signature);
        // 创建一个小型的存根函数, 假设参数已在栈上, 然后调用方法.
        const func = vm.newFunction(null, numParams + 1);

        var value = vm.makeHandle(func.asObj().toVal());
        value.value = vm.newClosure(func).asObj().toVal();
        func.code.push(OpCode.CODE_CALL_0.poffset(numParams).num());
        func.code.push(@as(u8, @intCast(method >> 8)) & 0xff);
        func.code.push(@as(u8, @intCast(method & 0xff)));
        func.code.push(OpCode.CODE_RETURN.num());
        func.code.push(OpCode.CODE_END.num());
        vm.functionBindName(func, signature);
        return value;
    }

    pub fn callHandle(vm: *@This(), method: ?*ZrenHandle) ZrenInterpretResult {
        Utils.assert(method != null, "Method cannot be NULL.");
        Utils.assert(method.?.value.isClosure(), "Method must be a method handle.");
        Utils.assert(vm.fiber != null, "Must set up arguments for call first.");
        Utils.assert(!vm.isApiStackNull(), "Must set up arguments for call first.");
        Utils.assert(vm.fiber.?.numFrames == 0, "Can not call from a foreign method.");

        const closure = method.?.value.asClosure();

        Utils.assert(vm.fiber.?.stack.top >= closure.func.arity, "");

        // 清理API栈. 现在call()已拥有控制权, 我们不再需要它.
        // 利用此值是否为非空来判断是否正在发生对外部方法的可重入调用，因此现在清除它非常重要，这样就可以在 call() 调用过程中调用外部方法.
        vm.clearApiStack();

        // 丢弃任何额外的临时槽位. 假设存根函数为每个参数都有一个槽位.
        vm.fiber.?.loadStack(closure.func.maxSlots);

        vm.callFunction(closure, 0);
        const result = vm.runInterpreter(vm.fiber.?);

        // 如果调用没有中止, 则设置API栈以指向栈的开头, 以便host可以访问调用的返回值.
        if (vm.fiber) |f| vm.setApiStack(f.stack.buffer, 0);

        return result;
    }

    pub fn makeHandle(vm: *@This(), value: Value) *ZrenHandle {
        if (value.isObj()) vm.pushRoot(value.asObj());

        // 构建句柄.
        var handle = vm.allocator.create(ZrenHandle) catch unreachable;

        handle.value = value;

        if (value.isObj()) vm.popRoot();

        // 将句柄添加到句柄链表的前面.
        if (vm.handles) |handles| handles.prev = handle;
        handle.prev = null;
        handle.next = vm.handles;
        vm.handles = handle;

        return handle;
    }

    fn defineClass(self: *@This(), module: *ObjModule, name: []const u8) *ObjClass {
        const nameString = self.newString(name).asString();
        self.pushRoot(nameString.asObj());

        const classObj = self.newSingleClass(0, nameString);
        _ = self.defineVariable(module, name, &classObj.asObj().toVal(), null);

        self.popRoot();
        return classObj;
    }

    // 绑定一个使用Zig函数[func]实现的名为[name]（在Zren中）的primitive方法到`ObjClass`[cls].
    fn primitive(self: *@This(), cls: *ObjClass, name: []const u8, func: fn (*@This(), []Value) bool) void {
        const symbol = self.symbolTableEnsure(&self.methodNames, name);
        const method: Method = .{ .method_type = .METHOD_PRIMITIVE, .as = .{ .primitive = func } };
        self.bindMethod(cls, symbol, method);
    }

    // 绑定一个使用Zig函数[func]实现的名为[name]（在Zren中）的primitive方法到`ObjClass`[cls]，但作为FN调用.
    fn functionCall(self: *@This(), cls: *ObjClass, name: []const u8, func: fn (*@This(), []Value) bool) void {
        const symbol = self.symbolTableEnsure(&self.methodNames, name);
        const method: Method = .{ .method_type = .METHOD_FUNCTION_CALL, .as = .{ .primitive = func } };
        self.bindMethod(cls, symbol, method);
    }

    inline fn runtimeError(self: *@This(), message: []const u8) void {
        self.fiber.?.err = self.newString(message);
    }
};

pub const ObjectModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.objectClass = vm.defineClass(module, "Object");
        vm.primitive(vm.objectClass, "!", objNot);
        vm.primitive(vm.objectClass, "==(_)", objectEqEq);
        vm.primitive(vm.objectClass, "!=(_)", objectBangQq);
        vm.primitive(vm.objectClass, "is(_)", objectIs);
        vm.primitive(vm.objectClass, "toString", objectToString);
        vm.primitive(vm.objectClass, "type", objectType);
    }

    pub fn objNot(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = .FALSE_VAL;
        return true;
    }

    pub fn objectEqEq(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (args[0].eql(args[1])) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn objectBangQq(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (!args[0].eql(args[1])) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn objectIs(vm: *ZrenVM, args: []Value) bool {
        if (!args[1].isClass()) {
            vm.runtimeError("Right operand must be a class.");
            return false;
        }

        var classObj = vm.getClass(args[0]);
        const baseClassObj = args[1].asClass();

        while (classObj) |clsObj| {
            if (@intFromPtr(baseClassObj) == @intFromPtr(clsObj)) {
                args[0] = .TRUE_VAL;
                return true;
            }

            classObj = clsObj.super_class;
        }
        args[0] = .FALSE_VAL;
        return true;
    }

    pub fn objectToString(vm: *ZrenVM, args: []Value) bool {
        const obj = args[0].asObj();
        const name = obj.class_obj.name.asObj().toVal();
        args[0] = vm.stringFormat("instance of @", .{name});
        return true;
    }

    pub fn objectType(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.getClass(args[0]).?.asObj().toVal();
        return true;
    }
};

pub const ClassModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.classClass = vm.defineClass(module, "Class");
        vm.bindSuperclass(vm.classClass, vm.objectClass);
        vm.primitive(vm.classClass, "name", className);
        vm.primitive(vm.classClass, "supertype", classSupertype);
        vm.primitive(vm.classClass, "toString", classToString);
        vm.primitive(vm.classClass, "attributes", classAttributes);
    }

    pub fn className(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = args[0].asClass().name.asObj().toVal();
        return true;
    }

    pub fn classSupertype(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (args[0].asClass().super_class) |s| s.asObj().toVal() else .NULL_VAL;
        return true;
    }

    pub fn classToString(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = args[0].asClass().name.asObj().toVal();
        return true;
    }

    pub fn classAttributes(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = args[0].asClass().attributes;
        return true;
    }
};

pub const MetaClassModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        const objectMetaclass = vm.defineClass(module, "Object metaclass");

        // 到这里所有的三个类已经定义了, 连接元类关系.
        vm.objectClass.asObj().class_obj = objectMetaclass;
        objectMetaclass.asObj().class_obj = vm.classClass;
        vm.classClass.asObj().class_obj = vm.classClass;

        // 在连接元类之后执行此操作，以便 objectMetaclass 不会被gc.
        vm.bindSuperclass(objectMetaclass, vm.classClass);
        vm.primitive(objectMetaclass, "same(_,_)", objectSame);

        // 核心类图最终看起来像这样，其中单线指向类的超类，双线指向其元类：
        //
        //        .------------------------------------. .====.
        //        |                  .---------------. | #    #
        //        v                  |               v | v    #
        //   .---------.   .-------------------.   .-------.  #
        //   | Object  |==>| Object metaclass  |==>| Class |=="
        //   '---------'   '-------------------'   '-------'
        //        ^                                 ^ ^ ^ ^
        //        |                  .--------------' # | #
        //        |                  |                # | #
        //   .---------.   .-------------------.      # | # -.
        //   |  Base   |==>|  Base metaclass   |======" | #  |
        //   '---------'   '-------------------'        | #  |
        //        ^                                     | #  |
        //        |                  .------------------' #  | Example classes
        //        |                  |                    #  |
        //   .---------.   .-------------------.          #  |
        //   | Derived |==>| Derived metaclass |=========="  |
        //   '---------'   '-------------------'            -'
    }

    pub fn objectSame(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (args[1].eql(args[2])) .TRUE_VAL else .FALSE_VAL;
        return true;
    }
};

pub const BoolModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.boolClass = vm.findVariable(module, "Bool").?.asClass();

        vm.primitive(vm.boolClass, "toString", boolToString);
        vm.primitive(vm.boolClass, "!", boolNot);
    }

    pub fn boolToString(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.newString(if (args[0].asBool()) "true" else "false");
        return true;
    }

    pub fn boolNot(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (!args[0].asBool()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }
};

pub const FiberModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.fiberClass = vm.findVariable(module, "Fiber").?.asClass();
        vm.primitive(vm.fiberClass.asObj().class_obj, "new(_)", fiberNew);
        vm.primitive(vm.fiberClass.asObj().class_obj, "abort(_)", fiberAbort);
        vm.primitive(vm.fiberClass.asObj().class_obj, "current", fiberCurrent);
        vm.primitive(vm.fiberClass.asObj().class_obj, "suspend()", fiberSuspend);
        vm.primitive(vm.fiberClass.asObj().class_obj, "yield()", fiberYield);
        vm.primitive(vm.fiberClass.asObj().class_obj, "yield(_)", fiberYield1);
        vm.primitive(vm.fiberClass, "call()", fiberCall);
        vm.primitive(vm.fiberClass, "call(_)", fiberCall1);
        vm.primitive(vm.fiberClass, "error", fiberError);
        vm.primitive(vm.fiberClass, "isDone", fiberIsDone);
        vm.primitive(vm.fiberClass, "transfer()", fiberTransfer);
        vm.primitive(vm.fiberClass, "transfer(_)", fiberTransfer1);
        vm.primitive(vm.fiberClass, "transferError(_)", fiberTransferError);
        vm.primitive(vm.fiberClass, "try()", fiberTry);
        vm.primitive(vm.fiberClass, "try(_)", fiberTry1);
    }

    pub fn fiberNew(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateFunc(args[1], "Argument")) return false;

        const closure = args[1].asClosure();
        if (closure.func.arity > 1) {
            vm.fiber.?.err = vm.newString("Function cannot take more than one parameter.");
            return false;
        }
        args[0] = vm.newFiber(closure).asObj().toVal();
        return true;
    }

    pub fn fiberAbort(vm: *ZrenVM, args: []Value) bool {
        vm.fiber.?.err = args[1];
        return args[1].isNull();
    }

    pub fn fiberCurrent(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.fiber.?.asObj().toVal();
        return true;
    }

    // 切换到 null 纤程已通知解释器停止并退出.
    pub fn fiberSuspend(vm: *ZrenVM, args: []Value) bool {
        _ = args;
        vm.fiber = null;
        vm.clearApiStack();
        return false;
    }

    pub fn fiberYield(vm: *ZrenVM, args: []Value) bool {
        _ = args;
        const current = vm.fiber orelse unreachable;
        vm.fiber = current.caller;

        // 将fiber从调用它的fiber中脱钩.
        current.caller = null;
        current.state = .FIBER_OTHER;

        // 让调用者的run方法返回null.
        if (vm.fiber) |f| f.peek(1).* = .NULL_VAL;
        return false;
    }

    pub fn fiberYield1(vm: *ZrenVM, args: []Value) bool {
        const current = vm.fiber orelse unreachable;
        vm.fiber = current.caller;

        // 将fiber从调用它的fiber中脱钩.
        current.caller = null;
        current.state = .FIBER_OTHER;
        if (vm.fiber) |f| {
            // 让调用者的run方法返回传递给yield的参数.
            f.peek(1).* = args[1];
            // 当被yielding的fiber恢复时，我们将存储yield调用的结果在其堆栈中.
            // 由于Fiber.yield(value) 有两个参数(Fiber类和value), 但只需要一个用于结果，丢弃另一个.
            current.stack.nOffset(1);
        }
        return false;
    }

    pub fn fiberCall(vm: *ZrenVM, args: []Value) bool {
        return vm.runFiber(args[0].asFiber(), args, true, false, "call");
    }

    pub fn fiberCall1(vm: *ZrenVM, args: []Value) bool {
        return vm.runFiber(args[0].asFiber(), args, true, true, "call");
    }

    pub fn fiberError(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = args[0].asFiber().err;
        return true;
    }

    pub fn fiberIsDone(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        const runFiber = args[0].asFiber();
        args[0] = if (runFiber.isDone()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn fiberTransfer(vm: *ZrenVM, args: []Value) bool {
        return vm.runFiber(args[0].asFiber(), args, false, false, "transfer to");
    }

    pub fn fiberTransfer1(vm: *ZrenVM, args: []Value) bool {
        return vm.runFiber(args[0].asFiber(), args, false, true, "transfer to");
    }

    pub fn fiberTransferError(vm: *ZrenVM, args: []Value) bool {
        _ = vm.runFiber(args[0].asFiber(), args, false, true, "transfer to");
        vm.fiber.?.err = args[1];
        return false;
    }

    pub fn fiberTry(vm: *ZrenVM, args: []Value) bool {
        _ = vm.runFiber(args[0].asFiber(), args, true, false, "try");

        // 如果正在切换到一个有效的fiber进行尝试，记录try状态
        if (!vm.fiber.?.hasErr()) vm.fiber.?.state = .FIBER_TRY;
        return false;
    }

    pub fn fiberTry1(vm: *ZrenVM, args: []Value) bool {
        _ = vm.runFiber(args[0].asFiber(), args, true, true, "try");

        // 如果正在切换到一个有效的fiber进行尝试，记录try状态
        if (!vm.fiber.?.hasErr()) vm.fiber.?.state = .FIBER_TRY;
        return false;
    }
};

pub const FuncModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.fnClass = vm.findVariable(module, "Fn").?.asClass();
        vm.primitive(vm.fnClass.asObj().class_obj, "new(_)", funcNew);
        vm.primitive(vm.fnClass, "arity", funcArity);
        vm.functionCall(vm.fnClass, "call()", funcCallN(0));
        vm.functionCall(vm.fnClass, "call(_)", funcCallN(1));
        vm.functionCall(vm.fnClass, "call(_,_)", funcCallN(2));
        vm.functionCall(vm.fnClass, "call(_,_,_)", funcCallN(3));
        vm.functionCall(vm.fnClass, "call(_,_,_,_)", funcCallN(4));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_)", funcCallN(5));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_)", funcCallN(6));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_)", funcCallN(7));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_)", funcCallN(8));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_)", funcCallN(9));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_,_)", funcCallN(10));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_,_,_)", funcCallN(11));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_,_,_,_)", funcCallN(12));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_,_,_,_,_)", funcCallN(13));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_,_,_,_,_,_)", funcCallN(14));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)", funcCallN(15));
        vm.functionCall(vm.fnClass, "call(_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_)", funcCallN(16));

        vm.primitive(vm.fnClass, "toString", funcToString);
    }

    pub fn funcNew(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateFunc(args[1], "Argument")) return false;

        args[0] = args[1]; // 该block参数已经是一个函数，所以直接返回它。
        return true;
    }

    pub fn funcArity(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@floatFromInt(args[0].asClosure().func.arity));
        return true;
    }

    pub inline fn funcCallN(comptime n: usize) fn (vm: *ZrenVM, args: []Value) bool {
        const Fn = struct {
            pub fn call(vm: *ZrenVM, args: []Value) bool {
                vm.callFunction(args[0].asClosure(), n + 1); // 这里+1是为了包含函数本身
                return false;
            }
        };
        return Fn.call;
    }

    pub fn funcToString(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.newString("<fn>");
        return true;
    }
};

pub const NullModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.nullClass = vm.findVariable(module, "Null").?.asClass();
        vm.primitive(vm.nullClass, "!", nullNot);
        vm.primitive(vm.nullClass, "toString", nullToString);
    }

    pub fn nullNot(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = .TRUE_VAL;
        return true;
    }

    pub fn nullToString(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.newString("null");
        return true;
    }
};

pub const NumModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.numClass = vm.findVariable(module, "Num").?.asClass();
        vm.primitive(vm.numClass.asObj().class_obj, "fromString(_)", numFromString);
        vm.primitive(vm.numClass.asObj().class_obj, "infinity", constNumFn(std.math.inf(f64)));
        vm.primitive(vm.numClass.asObj().class_obj, "nan", constNumFn(std.math.nan(f64)));
        vm.primitive(vm.numClass.asObj().class_obj, "pi", constNumFn(3.14159265358979323846264338327950288));
        vm.primitive(vm.numClass.asObj().class_obj, "tau", constNumFn(6.28318530717958647692528676655900577));
        vm.primitive(vm.numClass.asObj().class_obj, "largest", constNumFn(std.math.floatMax(f64)));
        vm.primitive(vm.numClass.asObj().class_obj, "smallest", constNumFn(std.math.floatMin(f64)));
        vm.primitive(vm.numClass.asObj().class_obj, "maxSafeInteger", constNumFn(9007199254740991.0));
        vm.primitive(vm.numClass.asObj().class_obj, "minSafeInteger", constNumFn(-9007199254740991.0));
        vm.primitive(vm.numClass, "-(_)", numMinus);
        vm.primitive(vm.numClass, "+(_)", numPlus);
        vm.primitive(vm.numClass, "*(_)", numMul);
        vm.primitive(vm.numClass, "/(_)", numDiv);
        vm.primitive(vm.numClass, "<(_)", numLt);
        vm.primitive(vm.numClass, ">(_)", numGt);
        vm.primitive(vm.numClass, "<=(_)", numLte);
        vm.primitive(vm.numClass, ">=(_)", numGte);
        vm.primitive(vm.numClass, "&(_)", numBitwiseAnd);
        vm.primitive(vm.numClass, "|(_)", numBitwiseOr);
        vm.primitive(vm.numClass, "^(_)", numBitwiseXor);
        vm.primitive(vm.numClass, "<<(_)", numBitwiseLeftShift);
        vm.primitive(vm.numClass, ">>(_)", numBitwiseRightShift);
        vm.primitive(vm.numClass, "abs", numAbs);
        vm.primitive(vm.numClass, "acos", numAcos);
        vm.primitive(vm.numClass, "asin", numAsin);
        vm.primitive(vm.numClass, "atan", numAtan);
        vm.primitive(vm.numClass, "cbrt", numCbrt);
        vm.primitive(vm.numClass, "ceil", numCeil);
        vm.primitive(vm.numClass, "cos", numCos);
        vm.primitive(vm.numClass, "floor", numFloor);
        vm.primitive(vm.numClass, "-", numNegate);
        vm.primitive(vm.numClass, "round", numRound);
        vm.primitive(vm.numClass, "min(_)", numMin);
        vm.primitive(vm.numClass, "max(_)", numMax);
        vm.primitive(vm.numClass, "clamp(_,_)", numClamp);
        vm.primitive(vm.numClass, "sin", numSin);
        vm.primitive(vm.numClass, "sqrt", numSqrt);
        vm.primitive(vm.numClass, "tan", numTan);
        vm.primitive(vm.numClass, "log", numLog);
        vm.primitive(vm.numClass, "log2", numLog2);
        vm.primitive(vm.numClass, "exp", numExp);
        vm.primitive(vm.numClass, "%(_)", numMod);
        vm.primitive(vm.numClass, "~", numBitwiseNot);
        vm.primitive(vm.numClass, "..(_)", numDotDot);
        vm.primitive(vm.numClass, "...(_)", numDotDotDot);
        vm.primitive(vm.numClass, "atan(_)", numAtan2);
        vm.primitive(vm.numClass, "pow(_)", numPow);
        vm.primitive(vm.numClass, "fraction", numFraction);
        vm.primitive(vm.numClass, "isInfinity", numIsInfinity);
        vm.primitive(vm.numClass, "isInteger", numIsInteger);
        vm.primitive(vm.numClass, "isNan", numIsNan);
        vm.primitive(vm.numClass, "sign", numSign);
        vm.primitive(vm.numClass, "toString", numToString);
        vm.primitive(vm.numClass, "truncate", numTruncate);

        // 这些定义只是为了使 0 和 -0 相等，这是 IEEE 754 所指定的，即使它们的位表示不同.
        vm.primitive(vm.numClass, "==(_)", numEqEq);
        vm.primitive(vm.numClass, "!=(_)", numBangEq);
    }

    pub fn numFromString(vm: *ZrenVM, args: []Value) bool {
        // TODO 可能有问题
        if (!vm.validateString(args[1], "Argument")) return false;
        const string = args[1].asString();
        if (string.value.len == 0) {
            args[0] = .NULL_VAL;
            return true;
        }
        // 解析字符串为数字
        const stripStr = std.mem.trim(u8, string.value, " ");
        const num = std.fmt.parseFloat(f64, stripStr) catch {
            // vm.fiber.?.err = vm.newString("Number literal is too large.");
            args[0] = .NULL_VAL;
            return true;
        };
        if (std.math.isNan(num) or std.math.isInf(num)) {
            vm.fiber.?.err = vm.newString("Number literal is too large.");
            return false;
        }
        args[0] = Value.numToValue(num);
        return true;
    }

    pub inline fn constNumFn(comptime num: anytype) fn (*ZrenVM, []Value) bool {
        const Fn = struct {
            fn call(vm: *ZrenVM, args: []Value) bool {
                _ = vm;
                args[0] = Value.numToValue(num);
                return true;
            }
        };
        return Fn.call;
    }

    pub fn numMinus(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = Value.numToValue(args[0].asNum() - args[1].asNum());
        return true;
    }

    pub fn numPlus(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = Value.numToValue(args[0].asNum() + args[1].asNum());
        return true;
    }

    pub fn numMul(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = Value.numToValue(args[0].asNum() * args[1].asNum());
        return true;
    }

    pub fn numDiv(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = Value.numToValue(args[0].asNum() / args[1].asNum());
        return true;
    }

    pub fn numLt(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = if (args[0].asNum() < args[1].asNum()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn numGt(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = if (args[0].asNum() > args[1].asNum()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn numLte(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = if (args[0].asNum() <= args[1].asNum()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn numGte(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = if (args[0].asNum() >= args[1].asNum()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn numBitwiseAnd(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        const left: u32 = @intFromFloat(args[0].asNum());
        const right: u32 = @intFromFloat(args[1].asNum());
        args[0] = Value.numToValue(@floatFromInt(left & right));
        return true;
    }

    pub fn numBitwiseOr(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        const left: u32 = @intFromFloat(args[0].asNum());
        const right: u32 = @intFromFloat(args[1].asNum());
        args[0] = Value.numToValue(@floatFromInt(left | right));
        return true;
    }

    pub fn numBitwiseXor(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        const left: u32 = @intFromFloat(args[0].asNum());
        const right: u32 = @intFromFloat(args[1].asNum());
        args[0] = Value.numToValue(@floatFromInt(left ^ right));
        return true;
    }

    pub fn numBitwiseLeftShift(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        const left: u32 = @intFromFloat(args[0].asNum());
        const right: u5 = @intFromFloat(args[1].asNum());
        args[0] = Value.numToValue(@floatFromInt(left << right));
        return true;
    }

    pub fn numBitwiseRightShift(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        const left: u32 = @intFromFloat(args[0].asNum());
        const right: u5 = @intFromFloat(args[1].asNum());
        args[0] = Value.numToValue(@floatFromInt(left >> right));
        return true;
    }

    pub fn numAbs(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@abs(args[0].asNum()));
        return true;
    }

    pub fn numAcos(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(std.math.acos(args[0].asNum()));
        return true;
    }

    pub fn numAsin(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(std.math.asin(args[0].asNum()));
        return true;
    }

    pub fn numAtan(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(std.math.atan(args[0].asNum()));
        return true;
    }

    pub fn numCbrt(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(std.math.cbrt(args[0].asNum()));
        return true;
    }

    pub fn numCeil(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@ceil(args[0].asNum()));
        return true;
    }

    pub fn numCos(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@cos(args[0].asNum()));
        return true;
    }

    pub fn numFloor(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@floor(args[0].asNum()));
        return true;
    }

    pub fn numNegate(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(-args[0].asNum());
        return true;
    }

    pub fn numRound(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@round(args[0].asNum()));
        return true;
    }

    pub fn numMin(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Other value")) return false;
        args[0] = Value.numToValue(@min(args[0].asNum(), args[1].asNum()));
        return true;
    }

    pub fn numMax(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Other value")) return false;
        args[0] = Value.numToValue(@max(args[0].asNum(), args[1].asNum()));
        return true;
    }

    pub fn numClamp(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Min value")) return false;
        if (!vm.validateNum(args[2], "Max value")) return false;
        args[0] = Value.numToValue(@min(@max(args[0].asNum(), args[1].asNum()), args[2].asNum()));
        return true;
    }

    pub fn numSin(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@sin(args[0].asNum()));
        return true;
    }

    pub fn numSqrt(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@sqrt(args[0].asNum()));
        return true;
    }

    pub fn numTan(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@tan(args[0].asNum()));
        return true;
    }

    pub fn numLog(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@log(args[0].asNum()));
        return true;
    }

    pub fn numLog2(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@log2(args[0].asNum()));
        return true;
    }

    pub fn numExp(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@exp(args[0].asNum()));
        return true;
    }

    pub fn numMod(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right operand")) return false;
        args[0] = Value.numToValue(@rem(args[0].asNum(), args[1].asNum()));
        return true;
    }

    // 位运算符总是在32位无符号整数上进行操作.
    pub fn numBitwiseNot(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        const left: u32 = @intFromFloat(args[0].asNum());
        args[0] = Value.numToValue(@floatFromInt(~left));
        return true;
    }

    pub fn numDotDot(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right hand side of range")) return false;
        const f = args[0].asNum();
        const t = args[1].asNum();
        args[0] = vm.newRange(f, t, true);
        return true;
    }

    pub fn numDotDotDot(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Right hand side of range")) return false;
        const f = args[0].asNum();
        const t = args[1].asNum();
        args[0] = vm.newRange(f, t, false);
        return true;
    }

    pub fn numAtan2(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "x value")) return false;
        args[0] = Value.numToValue(std.math.atan2(args[0].asNum(), args[1].asNum()));
        return true;
    }

    pub fn numPow(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateNum(args[1], "Power value")) return false;
        args[0] = Value.numToValue(std.math.pow(f64, args[0].asNum(), args[1].asNum()));
        return true;
    }

    const c = @cImport({
        @cInclude("math.h");
    });

    pub fn numFraction(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        //  这里仅为对齐原实现 , zig中可使用 @mod(args[0].asNum(), -1);
        var unused: f64 = undefined;
        args[0] = Value.numToValue(c.modf(args[0].asNum(), &unused));
        return true;
    }

    pub fn numIsInfinity(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (std.math.isInf(args[0].asNum())) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn numIsInteger(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        const v = args[0].asNum();
        if (std.math.isNan(v) or std.math.isInf(v)) {
            args[0] = .FALSE_VAL;
        } else {
            args[0] = if (@trunc(v) == v) .TRUE_VAL else .FALSE_VAL;
        }
        return true;
    }

    pub fn numIsNan(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (std.math.isNan(args[0].asNum())) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn numSign(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        const v = args[0].asNum();
        if (v > 0) {
            args[0] = Value.numToValue(1);
        } else if (v < 0) {
            args[0] = Value.numToValue(-1);
        } else {
            args[0] = Value.numToValue(0);
        }
        return true;
    }

    pub fn numToString(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.numToString(args[0].asNum());
        return true;
    }

    pub fn numTruncate(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@trunc(args[0].asNum()));
        return true;
    }

    pub fn numEqEq(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (args[1].isNum() and args[0].asNum() == args[1].asNum()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn numBangEq(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (!args[1].isNum() or args[0].asNum() != args[1].asNum()) .TRUE_VAL else .FALSE_VAL;
        return true;
    }
};

pub const StringModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.stringClass = vm.findVariable(module, "String").?.asClass();
        vm.primitive(vm.stringClass.asObj().class_obj, "fromCodePoint(_)", stringFromCodePoint);
        vm.primitive(vm.stringClass.asObj().class_obj, "fromByte(_)", stringFromByte);
        vm.primitive(vm.stringClass, "+(_)", stringPlus);
        vm.primitive(vm.stringClass, "[_]", stringSubscript);
        vm.primitive(vm.stringClass, "byteAt_(_)", stringByteAt);
        vm.primitive(vm.stringClass, "byteCount_", stringByteCount);
        vm.primitive(vm.stringClass, "codePointAt_(_)", stringCodePointAt);
        vm.primitive(vm.stringClass, "contains(_)", stringContains);
        vm.primitive(vm.stringClass, "endsWith(_)", stringEndsWith);
        vm.primitive(vm.stringClass, "indexOf(_)", stringIndexOf1);
        vm.primitive(vm.stringClass, "indexOf(_,_)", stringIndexOf2);
        vm.primitive(vm.stringClass, "iterate(_)", stringIterate);
        vm.primitive(vm.stringClass, "iterateByte_(_)", stringIterateByte);
        vm.primitive(vm.stringClass, "iteratorValue(_)", stringIteratorValue);
        vm.primitive(vm.stringClass, "startsWith(_)", stringStartsWith);
        vm.primitive(vm.stringClass, "toString", stringToString);
    }

    fn stringFromCodePoint(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateInt(args[1], "Code point")) return false;
        const codePoint: i64 = @intFromFloat(args[1].asNum());
        if (codePoint < 0) {
            vm.fiber.?.err = vm.newString("Code point cannot be negative.");
            return false;
        }

        if (codePoint > 0x0010ffff) {
            vm.fiber.?.err = vm.newString("Code point cannot be greater than 0x10ffff.");
            return false;
        }

        args[0] = vm.stringFromCodePoint(@intCast(codePoint));
        return true;
    }

    fn stringFromByte(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateInt(args[1], "Byte")) return false;
        const byte: i64 = @intFromFloat(args[1].asNum());

        if (byte < 0x00) {
            vm.fiber.?.err = vm.newString("Byte cannot be negative.");
            return false;
        }

        if (byte > 0xff) {
            vm.fiber.?.err = vm.newString("Byte cannot be greater than 0xff.");
            return false;
        }

        args[0] = vm.stringFromByte(@intCast(byte));
        return true;
    }

    pub fn stringPlus(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateString(args[1], "Right operand")) return false;
        args[0] = vm.stringFormat("@@", .{ args[0], args[1] });
        return true;
    }

    pub fn stringSubscript(vm: *ZrenVM, args: []Value) bool {
        const string = args[0].asString();

        if (args[1].isNum()) {
            const i = vm.validateIndex(args[1], string.value.len, "Subscript") orelse return false;
            args[0] = vm.stringCodePointAt(string, i);
            return true;
        }

        if (!args[1].isRange()) {
            vm.fiber.?.err = vm.newString("Subscript must be a number or a range.");
            return false;
        }

        var step: i32 = 0;
        var count: u32 = @intCast(string.value.len);
        const start = vm.calculateRange(args[1].asRange(), &count, &step) orelse return false;

        args[0] = vm.newStringFromRange(string, start, count, step);
        return true;
    }

    pub fn stringByteAt(vm: *ZrenVM, args: []Value) bool {
        const string = args[0].asString();
        const i = vm.validateIndex(args[1], string.value.len, "Index") orelse return false;
        args[0] = Value.numToValue(@floatFromInt(string.value[i]));
        return true;
    }

    pub fn stringByteCount(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@floatFromInt(args[0].asString().value.len));
        return true;
    }

    pub fn stringCodePointAt(vm: *ZrenVM, args: []Value) bool {
        const string = args[0].asString();
        const i = vm.validateIndex(args[1], string.value.len, "Index") orelse return false;
        if ((string.value[i] & 0xc0) == 0x80) {
            args[0] = Value.numToValue(-1);
        } else {
            // TODO 改进
            // 解码UTF-8序列.
            if (Utils.utf8Decode(string.value[i..])) |v| {
                args[0] = Value.numToValue(@floatFromInt(v));
            } else {
                args[0] = Value.numToValue(-1);
            }
        }
        return true;
    }

    pub fn stringContains(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateString(args[1], "Argument")) return false;
        const string = args[0].asString();
        const search = args[1].asString();
        args[0] = if (string.strContains(search, 0)) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn stringEndsWith(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateString(args[1], "Argument")) return false;
        const string = args[0].asString();
        const search = args[1].asString();
        args[0] = if (string.endsWith(search)) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn stringIndexOf1(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateString(args[1], "Argument")) return false;

        const string = args[0].asString();
        const search = args[1].asString();

        if (string.strFind(search, 0)) |i| {
            args[0] = Value.numToValue(@floatFromInt(i));
        } else {
            args[0] = Value.numToValue(-1);
        }
        return true;
    }

    pub fn stringIndexOf2(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateString(args[1], "Argument")) return false;

        const string = args[0].asString();
        const search = args[1].asString();

        const start = vm.validateIndex(args[2], string.value.len, "Start") orelse return false;

        if (string.strFind(search, @intCast(start))) |i| {
            args[0] = Value.numToValue(@floatFromInt(start + i));
        } else {
            args[0] = Value.numToValue(-1);
        }
        return true;
    }

    pub fn stringIterate(vm: *ZrenVM, args: []Value) bool {
        const string = args[0].asString();
        if (args[1].isNull()) {
            if (string.value.len == 0) {
                args[0] = .FALSE_VAL;
            } else {
                args[0] = Value.numToValue(0);
            }
            return true;
        }

        if (!vm.validateInt(args[1], "Iterator")) return false;

        if (args[1].asNum() < 0) {
            args[0] = .FALSE_VAL;
            return true;
        }
        const start: usize = @intFromFloat(args[1].asNum() + 1);
        for (start..@max(string.value.len, start)) |i| {
            if ((string.value[i] & 0xc0) == 0x80) continue;
            args[0] = Value.numToValue(@floatFromInt(i));
            return true;
        }
        args[0] = .FALSE_VAL;
        return true;
    }

    pub fn stringIterateByte(vm: *ZrenVM, args: []Value) bool {
        const string = args[0].asString();
        if (args[1].isNull()) {
            if (string.value.len == 0) {
                args[0] = .FALSE_VAL;
            } else {
                args[0] = Value.numToValue(0);
            }
            return true;
        }

        if (!vm.validateInt(args[1], "Iterator")) return false;

        if (args[1].asNum() < 0) {
            args[0] = .FALSE_VAL;
            return true;
        }

        // 前移到下一个字节.
        const i: usize = @intFromFloat(args[1].asNum() + 1);
        if (i >= string.value.len) {
            args[0] = .FALSE_VAL;
        } else {
            args[0] = Value.numToValue(@floatFromInt(i));
        }
        return true;
    }

    pub fn stringIteratorValue(vm: *ZrenVM, args: []Value) bool {
        const string = args[0].asString();
        const i = vm.validateIndex(args[1], string.value.len, "Iterator") orelse return false;
        args[0] = vm.stringCodePointAt(string, i);
        return true;
    }

    pub fn stringStartsWith(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateString(args[1], "Argument")) return false;
        const string = args[0].asString();
        const search = args[1].asString();
        args[0] = if (string.startsWith(search)) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn stringToString(vm: *ZrenVM, args: []Value) bool {
        _ = .{ vm, args };
        return true;
    }
};

pub const ListModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.listClass = vm.findVariable(module, "List").?.asClass();
        vm.primitive(vm.listClass.asObj().class_obj, "filled(_,_)", listFilled);
        vm.primitive(vm.listClass.asObj().class_obj, "new()", listNew);
        vm.primitive(vm.listClass, "[_]", listSubscript);
        vm.primitive(vm.listClass, "[_]=(_)", listSubscriptSetter);
        vm.primitive(vm.listClass, "add(_)", listAdd);
        vm.primitive(vm.listClass, "addCore_(_)", listAddCore);
        vm.primitive(vm.listClass, "clear()", listClear);
        vm.primitive(vm.listClass, "count", listCount);
        vm.primitive(vm.listClass, "insert(_,_)", listInsert);
        vm.primitive(vm.listClass, "iterate(_)", listIterate);
        vm.primitive(vm.listClass, "iteratorValue(_)", listIteratorValue);
        vm.primitive(vm.listClass, "removeAt(_)", listRemoveAt);
        vm.primitive(vm.listClass, "remove(_)", listRemoveValue);
        vm.primitive(vm.listClass, "indexOf(_)", listIndexOf);
        vm.primitive(vm.listClass, "swap(_,_)", listSwap);
    }

    pub fn listFilled(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateInt(args[1], "Size")) return false;
        if (args[1].asNum() < 0) {
            vm.fiber.?.err = vm.newString("Size cannot be negative.");
            return false;
        }
        const size: usize = @intFromFloat(args[1].asNum());
        const list = vm.newList(size);
        for (0..size) |i| {
            list.elements.rat(i).* = args[2];
        }
        args[0] = list.asObj().toVal();
        return true;
    }

    pub fn listNew(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.newList(0).asObj().toVal();
        return true;
    }

    pub fn listSubscript(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        if (args[1].isNum()) {
            const i = vm.validateIndex(args[1], list.elements.count, "Subscript") orelse return false;
            args[0] = list.elements.at(i);
            return true;
        }

        if (!args[1].isRange()) {
            vm.fiber.?.err = vm.newString("Subscript must be a number or a range.");
            return false;
        }

        var step: i32 = 0;
        var count: u32 = @intCast(list.elements.count);
        const start = vm.calculateRange(args[1].asRange(), &count, &step) orelse return false;
        const result = vm.newList(count);

        for (0..count) |i| {
            const index = Utils.calcRangeIndex(start, i, step);
            result.elements.rat(i).* = list.elements.at(index);
        }

        args[0] = result.asObj().toVal();
        return true;
    }

    pub fn listSubscriptSetter(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        const i = vm.validateIndex(args[1], list.elements.count, "Subscript") orelse return false;
        list.elements.rat(i).* = args[2];
        args[0] = args[2];
        return true;
    }

    pub fn listAdd(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0].asList().elements.push(args[1]);
        args[0] = args[1];
        return true;
    }

    pub fn listAddCore(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0].asList().elements.push(args[1]);
        return true;
    }

    pub fn listClear(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0].asList().elements.clear();
        args[0] = .NULL_VAL;
        return true;
    }

    pub fn listCount(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@floatFromInt(args[0].asList().elements.count));
        return true;
    }

    pub fn listInsert(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        const i = vm.validateIndex(args[1], list.elements.count + 1, "Index") orelse return false;
        vm.listInsert(list, args[2], i);
        args[0] = args[2];
        return true;
    }

    pub fn listIterate(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        if (args[1].isNull()) {
            args[0] = if (list.elements.count == 0) .FALSE_VAL else Value.numToValue(0);
            return true;
        }

        if (!vm.validateInt(args[1], "Iterator")) return false;

        // 如果超出范围, 则停止.
        const index = args[1].asNum();
        if (index < 0 or index >= @as(f64, @floatFromInt(list.elements.count - 1))) {
            args[0] = .FALSE_VAL;
            return true;
        }
        // 否则, 移动到下一个索引.
        args[0] = Value.numToValue(index + 1);
        return true;
    }

    pub fn listIteratorValue(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        const i = vm.validateIndex(args[1], list.elements.count, "Iterator") orelse return false;
        args[0] = list.elements.at(i);
        return true;
    }

    pub fn listRemoveAt(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        const i = vm.validateIndex(args[1], list.elements.count, "Index") orelse return false;
        args[0] = vm.listRemoveAt(list, i);
        return true;
    }

    pub fn listRemoveValue(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        args[0] = if (vm.listIndexOf(list, args[1])) |i| vm.listRemoveAt(list, i) else .NULL_VAL;
        return true;
    }

    pub fn listIndexOf(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        if (vm.listIndexOf(list, args[1])) |i| {
            args[0] = Value.numToValue(@floatFromInt(i));
        } else {
            args[0] = Value.numToValue(-1);
        }
        return true;
    }

    pub fn listSwap(vm: *ZrenVM, args: []Value) bool {
        const list = args[0].asList();
        const indexA = vm.validateIndex(args[1], list.elements.count, "Index 0") orelse return false;
        const indexB = vm.validateIndex(args[2], list.elements.count, "Index 1") orelse return false;

        const a = list.elements.rat(indexA).*;
        list.elements.rat(indexA).* = list.elements.at(indexB);
        list.elements.rat(indexB).* = a;

        args[0] = .NULL_VAL;
        return true;
    }
};

pub const MapModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.mapClass = vm.findVariable(module, "Map").?.asClass();
        vm.primitive(vm.mapClass.asObj().class_obj, "new()", mapNew);
        vm.primitive(vm.mapClass, "[_]", mapSubscript);
        vm.primitive(vm.mapClass, "[_]=(_)", mapSubscriptSetter);
        vm.primitive(vm.mapClass, "addCore_(_,_)", mapAddCore);
        vm.primitive(vm.mapClass, "clear()", mapClear);
        vm.primitive(vm.mapClass, "containsKey(_)", mapContainsKey);
        vm.primitive(vm.mapClass, "count", mapCount);
        vm.primitive(vm.mapClass, "remove(_)", mapRemove);
        vm.primitive(vm.mapClass, "iterate(_)", mapIterate);
        vm.primitive(vm.mapClass, "keyIteratorValue_(_)", mapKeyIteratorValue);
        vm.primitive(vm.mapClass, "valueIteratorValue_(_)", mapValueIteratorValue);
    }

    pub fn mapNew(vm: *ZrenVM, args: []Value) bool {
        args[0] = vm.newMap().asObj().toVal();
        return true;
    }

    pub fn mapSubscript(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateKey(args[1])) return false;
        const map = args[0].asMap();
        const value = map.mapGet(args[1]);
        args[0] = if (value.isUndefined()) .NULL_VAL else value;
        return true;
    }

    pub fn mapSubscriptSetter(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateKey(args[1])) return false;
        args[0].asMap().mapSet(args[1], args[2]);
        args[0] = args[2];
        return true;
    }

    // 添加一个条目到map中, 然后返回map本身.
    // 该方法用于在编译map字面量时调用, 而不是使用[_]=(_)来减少堆栈的消耗.
    pub fn mapAddCore(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateKey(args[1])) return false;
        args[0].asMap().mapSet(args[1], args[2]);
        return true;
    }

    pub fn mapClear(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0].asMap().clear();
        args[0] = .NULL_VAL;
        return true;
    }

    pub fn mapContainsKey(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateKey(args[1])) return false;
        args[0] = if (args[0].asMap().mapContains(args[1])) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn mapCount(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(@floatFromInt(args[0].asMap().mapCount()));
        return true;
    }

    pub fn mapRemove(vm: *ZrenVM, args: []Value) bool {
        if (!vm.validateKey(args[1])) return false;
        args[0] = vm.mapRemoveKey(args[0].asMap(), (args[1]));
        return true;
    }

    pub fn mapIterate(vm: *ZrenVM, args: []Value) bool {
        // TODO 可能有问题
        const map = args[0].asMap();
        if (map.count() == 0) {
            args[0] = .FALSE_VAL;
            return true;
        }
        var iter = map.entries.iterator();
        if (!args[1].isNull()) {
            if (!vm.validateInt(args[1], "Iterator")) return false;
            const index = args[1].asNum();
            if (index < 0) {
                args[0] = .FALSE_VAL;
                return true;
            }
            iter.index = @as(u32, @intFromFloat(index));
            if (iter.index >= map.capacity()) {
                args[0] = .FALSE_VAL;
                return true;
            }
            iter.index += 1;
        }
        args[0] = if (iter.next()) |_| Value.numToValue(@floatFromInt(iter.index - 1)) else .FALSE_VAL;
        return true;
    }

    pub fn mapKeyIteratorValue(vm: *ZrenVM, args: []Value) bool {
        const map = args[0].asMap();
        const i = vm.validateIndex(args[1], map.capacity(), "Iterator") orelse return false;
        const iterator = map.entries.keyIterator();
        // TODO 是否需要空判断
        args[0] = iterator.items[i];
        return true;
    }

    pub fn mapValueIteratorValue(vm: *ZrenVM, args: []Value) bool {
        const map = args[0].asMap();
        const i = vm.validateIndex(args[1], map.capacity(), "Iterator") orelse return false;
        const iterator = map.entries.valueIterator();
        // TODO 是否需要空判断
        args[0] = iterator.items[i];
        return true;
    }
};

pub const RangeModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        vm.rangeClass = vm.findVariable(module, "Range").?.asClass();
        vm.primitive(vm.rangeClass, "from", rangeFrom);
        vm.primitive(vm.rangeClass, "to", rangeTo);
        vm.primitive(vm.rangeClass, "min", rangeMin);
        vm.primitive(vm.rangeClass, "max", rangeMax);
        vm.primitive(vm.rangeClass, "isInclusive", rangeIsInclusive);
        vm.primitive(vm.rangeClass, "iterate(_)", rangeIterate);
        vm.primitive(vm.rangeClass, "iteratorValue(_)", rangeIteratorValue);
        vm.primitive(vm.rangeClass, "toString", rangeToString);
    }

    pub fn rangeFrom(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(args[0].asRange().from);
        return true;
    }

    pub fn rangeTo(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = Value.numToValue(args[0].asRange().to);
        return true;
    }

    pub fn rangeMin(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        const range = args[0].asRange();
        args[0] = Value.numToValue(@min(range.from, range.to));
        return true;
    }

    pub fn rangeMax(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        const range = args[0].asRange();
        args[0] = Value.numToValue(@max(range.from, range.to));
        return true;
    }

    pub fn rangeIsInclusive(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = if (args[0].asRange().isInclusive) .TRUE_VAL else .FALSE_VAL;
        return true;
    }

    pub fn rangeIterate(vm: *ZrenVM, args: []Value) bool {
        const range = args[0].asRange();

        // 特殊情况: 空范围.
        if (range.from == range.to and !range.isInclusive) {
            args[0] = .FALSE_VAL;
            return true;
        }

        // 开始迭代.
        if (args[1].isNull()) {
            args[0] = Value.numToValue(range.from);
            return true;
        }

        if (!vm.validateNum(args[1], "Iterator")) return false;

        var iterator = args[1].asNum();

        // 从from迭代到to.
        if (range.from < range.to) {
            iterator += 1;
            if (iterator > range.to) {
                args[0] = .FALSE_VAL;
                return true;
            }
        } else {
            iterator -= 1;
            if (iterator < range.to) {
                args[0] = .FALSE_VAL;
                return true;
            }
        }

        if (!range.isInclusive and iterator == range.to) {
            args[0] = .FALSE_VAL;
            return true;
        }
        args[0] = Value.numToValue(iterator);
        return true;
    }

    pub fn rangeIteratorValue(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        args[0] = args[1]; // 假设迭代器是一个数字, 所以其是range的value
        return true;
    }

    pub fn rangeToString(vm: *ZrenVM, args: []Value) bool {
        const range = args[0].asRange();

        const f = vm.numToString(range.from);
        vm.pushRoot(f.asObj());

        const t = vm.numToString(range.to);
        vm.pushRoot(t.asObj());

        const result = vm.stringFormat("@$@", .{ f, if (range.isInclusive) ".." else "...", t });

        vm.popRoot();
        vm.popRoot();
        args[0] = result;
        return true;
    }
};

pub const SystemModule = struct {
    pub fn _install(vm: *ZrenVM, module: *ObjModule) void {
        const systemClass = vm.findVariable(module, "System").?.asClass();
        vm.primitive(systemClass.asObj().class_obj, "clock", systemClock);
        vm.primitive(systemClass.asObj().class_obj, "gc()", systemGc);
        vm.primitive(systemClass.asObj().class_obj, "writeString_(_)", systemWriteString);
    }

    pub fn systemClock(vm: *ZrenVM, args: []Value) bool {
        _ = vm;
        const us: f64 = @floatFromInt(std.time.microTimestamp());
        args[0] = Value.numToValue(us * 1e-6);
        return true;
    }

    pub fn systemGc(vm: *ZrenVM, args: []Value) bool {
        vm.collectGarbage();
        args[0] = .NULL_VAL;
        return true;
    }

    pub fn systemWriteString(vm: *ZrenVM, args: []Value) bool {
        if (vm.config.writeFn) |f| f(vm, args[1].asString().value);
        args[0] = args[1];
        return true;
    }
};
