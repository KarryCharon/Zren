const std = @import("std");
const build_options = @import("build_options");
const print = std.debug.print;
const Allocator = std.mem.Allocator;
const GenericBuffer = @import("./buffer.zig").GenericBuffer;
const GenericStack = @import("./stack.zig").GenericStack;

const Utils = @import("./utils.zig");
// 根据编译选项选择实现
pub const Value = if (build_options.nan_tagging) ValueNaNTagged else ValueTraditional;
pub const ByteBuffer = GenericBuffer(u8);
pub const UsizeBuffer = GenericBuffer(usize);
pub const StringBuffer = GenericBuffer(*ObjString);
pub const ValueBuffer = GenericBuffer(Value);
pub const MethodBuffer = @import("./method.zig").MethodBuffer;

pub const OpCode = @import("./opcode.zig").OpCode;

pub const SymbolTable = StringBuffer;

pub const FuncDebug = struct {
    name: []u8,
    source_lines: UsizeBuffer,
};

pub const CallFrame = struct {
    ip: []u8,
    closure: *ObjClosure,
    stack_ref: []Value,
    stack_offset: usize = 0,
    ip_offset: usize = 0,

    pub inline fn ipPosOffset(self: *@This(), i: u16) void {
        self.ip_offset += i;
    }

    pub inline fn ipNegOffset(self: *@This(), i: u16) void {
        self.ip_offset -= i;
    }

    pub inline fn rbyte(self: *@This()) u8 {
        self.ipPosOffset(1);
        return self.ip[self.ip_offset - 1];
    }

    pub inline fn rshort(self: *@This()) u16 {
        self.ipPosOffset(2);
        return Utils.b8Tob16(self.ip[self.ip_offset - 2], self.ip[self.ip_offset - 1]);
    }

    pub inline fn setStackRef(self: *@This(), ref: []Value) void {
        self.stack_ref = ref;
    }

    pub inline fn setStack(self: *@This(), stack: []Value, offset: usize) void {
        self.stack_ref = stack;
        self.stack_offset = offset;
    }

    pub inline fn stackAt(self: *@This(), i: usize) *Value {
        return &self.stack_ref[self.stack_offset + i];
    }

    pub inline fn writeStack(self: *@This(), i: usize, v: *const Value) void {
        self.stack_ref[self.stack_offset + i] = v.*;
    }

    pub inline fn readConstant(self: *@This(), i: usize) *Value {
        return self.closure.func.constants.rat(i);
    }

    pub inline fn readUpvalue(self: *@This(), i: usize) *Value {
        return self.closure.upvalues[i].?.value;
    }

    pub inline fn writeUpvalue(self: *@This(), i: usize, v: *Value) void {
        self.closure.upvalues[i].?.value.* = v.*;
    }

    pub inline fn readModuleVar(self: *@This(), i: usize) *Value {
        return self.closure.func.module.?.variables.rat(i);
    }

    pub inline fn writeModuleVar(self: *@This(), i: usize, v: *Value) void {
        self.closure.func.module.?.variables.rat(i).* = v.*;
    }
};

pub const FiberState = enum {
    FIBER_TRY,
    FIBER_ROOT,
    FIBER_OTHER,
};

pub const ValueHashContext = struct {
    pub fn hash(self: @This(), k: Value) u64 {
        _ = self;
        return k.hashCode();
    }

    pub fn eql(self: @This(), a: Value, b: Value) bool {
        _ = self;
        return a.eql(b);
    }
};

pub const ValueHashMap = std.HashMap(Value, Value, ValueHashContext, 75);
pub const AutoValueHashMap = std.AutoHashMap(Value, Value);

pub const ValueNaNTagged = struct {
    pub const Self = @This();
    pub const SIGN_BIT: u64 = 1 << 63; // 用于获取符号位的掩码.
    pub const QNAN: u64 = 0x7ffc000000000000; // 用于验证是否为NaN

    pub const TAG_MASK: u64 = 0b111;

    pub const TAG_NAN: u64 = 0;
    pub const TAG_NULL: u64 = 1;
    pub const TAG_FALSE: u64 = 2;
    pub const TAG_TRUE: u64 = 3;
    pub const TAG_UNDEFINED: u64 = 4;
    pub const TAG_UNUSED2: u64 = 5;
    pub const TAG_UNUSED3: u64 = 6;
    pub const TAG_UNUSED4: u64 = 7;

    pub const NULL_VAL: Self = .{ .data = QNAN | TAG_NULL };
    pub const FALSE_VAL: Self = .{ .data = QNAN | TAG_FALSE };
    pub const TRUE_VAL: Self = .{ .data = QNAN | TAG_TRUE };
    pub const UNDEFINED_VAL: Self = .{ .data = QNAN | TAG_UNDEFINED };

    data: u64 = 0,

    pub inline fn getTag(self: Self) u64 {
        return @intCast(self.data & TAG_MASK);
    }

    pub inline fn numToValue(num: f64) Self {
        return .{ .data = @bitCast(num) };
    }

    pub inline fn fromObj(obj: *Obj) Self {
        return .{ .data = SIGN_BIT | QNAN | @intFromPtr(obj) };
    }

    pub fn dumpVal(self: Self) void {
        if (self.isNum()) return print("{d:14}", .{self.asNum()});
        if (self.isObj()) return self.asObj().dumpObj();
        switch (self.getTag()) {
            TAG_FALSE => print("false", .{}),
            TAG_NAN => print("NaN", .{}),
            TAG_NULL => print("null", .{}),
            TAG_TRUE => print("true", .{}),
            TAG_UNDEFINED => print("undefined", .{}),
            else => unreachable,
        }
    }

    pub inline fn hashCode(self: Self) u64 {
        return if (self.isObj()) self.asObj().hashCode() else Utils.hashBits(self.data);
    }

    pub fn eql(self: Self, other: Self) bool {
        if (self.valueSame(other)) return true;
        if (!self.isObj() or !other.isObj()) return false;
        return self.asObj().eql(other.asObj());
    }

    pub inline fn valueSame(self: Self, other: Self) bool {
        return std.meta.eql(self, other);
    }

    pub inline fn asClass(self: Self) *ObjClass {
        return self.asObj().asClass();
    }

    pub inline fn asClosure(self: Self) *ObjClosure {
        return self.asObj().asClosure();
    }

    pub inline fn asFiber(self: Self) *ObjFiber {
        return self.asObj().asFiber();
    }

    pub inline fn asFunc(self: Self) *ObjFunc {
        return self.asObj().asFunc();
    }

    pub inline fn asForeign(self: Self) *ObjForeign {
        return self.asObj().asForeign();
    }

    pub inline fn asInstance(self: Self) *ObjInstance {
        return self.asObj().asInstance();
    }

    pub inline fn asList(self: Self) *ObjList {
        return self.asObj().asList();
    }

    pub inline fn asMap(self: Self) *ObjMap {
        return self.asObj().asMap();
    }

    pub inline fn asModule(self: Self) *ObjModule {
        return self.asObj().asModule();
    }

    pub inline fn asNum(self: Self) f64 {
        return @bitCast(self.data);
    }

    pub inline fn asBool(self: Self) bool {
        return self.valueSame(TRUE_VAL);
    }

    pub inline fn asObj(self: Self) *Obj {
        return @ptrFromInt(self.data & ~(SIGN_BIT | QNAN));
    }

    pub inline fn asRange(self: Self) *ObjRange {
        return self.asObj().asRange();
    }

    pub inline fn asString(self: Self) *ObjString {
        return self.asObj().asString();
    }

    pub inline fn asCString(self: Self) []u8 {
        return self.asObj().asString().value;
    }

    pub fn isObjType(self: Self, obj_type: ObjType) bool {
        return self.isObj() and self.asObj().isObjType(obj_type);
    }

    pub fn isValidKey(self: Self) bool {
        return self.isBool() or self.isClass() or self.isNull() or self.isNum() or self.isRange() or self.isString();
    }

    pub inline fn isBool(self: Self) bool {
        return self.valueSame(FALSE_VAL) or self.valueSame(TRUE_VAL);
    }

    pub inline fn isClass(self: Self) bool {
        return self.isObjType(.OBJ_CLASS);
    }

    pub inline fn isClosure(self: Self) bool {
        return self.isObjType(.OBJ_CLOSURE);
    }

    pub inline fn isFiber(self: Self) bool {
        return self.isObjType(.OBJ_FIBER);
    }

    pub inline fn isFunc(self: Self) bool {
        return self.isObjType(.OBJ_FUNC);
    }

    pub inline fn isForeign(self: Self) bool {
        return self.isObjType(.OBJ_FOREIGN);
    }

    pub inline fn isInstance(self: Self) bool {
        return self.isObjType(.OBJ_INSTANCE);
    }

    pub inline fn isList(self: Self) bool {
        return self.isObjType(.OBJ_LIST);
    }

    pub inline fn isMap(self: Self) bool {
        return self.isObjType(.OBJ_MAP);
    }

    pub inline fn isRange(self: Self) bool {
        return self.isObjType(.OBJ_RANGE);
    }

    pub inline fn isString(self: Self) bool {
        return self.isObjType(.OBJ_STRING);
    }

    // 如果NaN位被设置, 则不是数字
    pub inline fn isNum(self: Self) bool {
        return (self.data & QNAN) != QNAN;
    }

    // 对于对象指针, NaN位被设置, 且符号位被设置
    pub inline fn isObj(self: Self) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub inline fn isFalse(self: Self) bool {
        return self.valueSame(FALSE_VAL) or self.valueSame(NULL_VAL);
    }

    pub inline fn isNull(self: Self) bool {
        return self.valueSame(NULL_VAL);
    }

    pub inline fn isUndefined(self: Self) bool {
        return self.valueSame(UNDEFINED_VAL);
    }
};

pub const ValueTraditional = struct {
    pub const Self = @This();
    pub const ValueType = enum {
        VAL_FALSE,
        VAL_NULL,
        VAL_NUM,
        VAL_TRUE,
        VAL_UNDEFINED,
        VAL_OBJ,
    };

    const Data = union(enum) {
        num: f64,
        obj: *Obj,
    };

    pub const NULL_VAL: Self = .{ .value_type = .VAL_NULL, .as = .{ .num = 0 } };
    pub const FALSE_VAL: Self = .{ .value_type = .VAL_FALSE, .as = .{ .num = 0 } };
    pub const TRUE_VAL: Self = .{ .value_type = .VAL_TRUE, .as = .{ .num = 0 } };
    pub const UNDEFINED_VAL: Self = .{ .value_type = .VAL_UNDEFINED, .as = .{ .num = 0 } };

    value_type: ValueType = undefined,
    as: Data = undefined,

    pub inline fn numToValue(num: f64) Self {
        return .{ .value_type = .VAL_NUM, .as = .{ .num = num } };
    }

    pub inline fn fromObj(obj: *Obj) Self {
        return .{ .value_type = .VAL_OBJ, .as = .{ .obj = obj } };
    }

    pub fn dumpVal(self: *@This()) void {
        switch (self.value_type) {
            .VAL_FALSE => print("false", .{}),
            .VAL_NULL => print("null", .{}),
            .VAL_NUM => print("{d:14}", .{self.asNum()}),
            .VAL_TRUE => print("true", .{}),
            .VAL_OBJ => self.asObj().dumpObj(),
            .VAL_UNDEFINED => unreachable,
        }
    }

    pub fn hashCode(self: *const @This()) u64 {
        switch (self.value_type) {
            .VAL_FALSE => return 0,
            .VAL_NULL => return 1,
            .VAL_NUM => return Utils.hashNum(self.asNum()),
            .VAL_TRUE => return 2,
            .VAL_OBJ => return self.asObj().hashCode(),
            .VAL_UNDEFINED => unreachable,
        }
        return 0;
    }

    pub fn eql(self: *const @This(), other: Self) bool {
        if (self.valueSame(other)) return true;
        if (!self.isObj() or !other.isObj()) return false;
        return self.asObj().eql(other.asObj());
    }

    pub fn valueSame(self: *const @This(), other: Self) bool {
        if (self.value_type != other.value_type) return false;
        if (self.isNum()) return self.asNum() == other.asNum();
        switch (self.as) {
            .obj => return self.as.obj == other.as.obj,
            .num => return self.as.num == other.as.num,
        }
    }

    pub inline fn asClass(self: *const @This()) *ObjClass {
        return self.asObj().asClass();
    }

    pub inline fn asClosure(self: *const @This()) *ObjClosure {
        return self.asObj().asClosure();
    }

    pub inline fn asFiber(self: *const @This()) *ObjFiber {
        return self.asObj().asFiber();
    }

    pub inline fn asFunc(self: *const @This()) *ObjFunc {
        return self.asObj().asFunc();
    }

    pub inline fn asForeign(self: *const @This()) *ObjForeign {
        return self.asObj().asForeign();
    }

    pub inline fn asInstance(self: *const @This()) *ObjInstance {
        return self.asObj().asInstance();
    }

    pub inline fn asList(self: *const @This()) *ObjList {
        return self.asObj().asList();
    }

    pub inline fn asMap(self: *const @This()) *ObjMap {
        return self.asObj().asMap();
    }

    pub inline fn asModule(self: *const @This()) *ObjModule {
        return self.asObj().asModule();
    }

    pub inline fn asNum(self: *const @This()) f64 {
        return self.as.num;
    }

    pub inline fn asBool(self: *const @This()) bool {
        return self.isTrue();
    }

    pub inline fn asRange(self: *const @This()) *ObjRange {
        return self.asObj().asRange();
    }

    pub inline fn asString(self: *const @This()) *ObjString {
        return self.asObj().asString();
    }

    pub inline fn asCString(self: *const @This()) []u8 {
        return self.asObj().asString().value;
    }

    pub inline fn asObj(self: *const @This()) *Obj {
        return self.as.obj;
    }

    pub inline fn isObjType(self: *const @This(), obj_type: ObjType) bool {
        return if (self.isObj()) self.asObj().isObjType(obj_type) else false;
    }

    pub fn isValidKey(self: *const @This()) bool {
        return self.isBool() or self.isClass() or self.isNull() or self.isNum() or self.isRange() or self.isString();
    }

    pub inline fn isBool(self: *const @This()) bool {
        return self.value_type == .VAL_TRUE or self.value_type == .VAL_FALSE;
    }

    pub inline fn isClass(self: *const @This()) bool {
        return self.isObjType(.OBJ_CLASS);
    }

    pub inline fn isClosure(self: *const @This()) bool {
        return self.isObjType(.OBJ_CLOSURE);
    }

    pub inline fn isFiber(self: *const @This()) bool {
        return self.isObjType(.OBJ_FIBER);
    }

    pub inline fn isFunc(self: *const @This()) bool {
        return self.isObjType(.OBJ_FUNC);
    }

    pub inline fn isForeign(self: *const @This()) bool {
        return self.isObjType(.OBJ_FOREIGN);
    }

    pub inline fn isInstance(self: *const @This()) bool {
        return self.isObjType(.OBJ_INSTANCE);
    }

    pub inline fn isList(self: *const @This()) bool {
        return self.isObjType(.OBJ_LIST);
    }

    pub inline fn isMap(self: *const @This()) bool {
        return self.isObjType(.OBJ_MAP);
    }

    pub inline fn isRange(self: *const @This()) bool {
        return self.isObjType(.OBJ_RANGE);
    }

    pub inline fn isString(self: *const @This()) bool {
        return self.isObjType(.OBJ_STRING);
    }

    pub inline fn isObj(self: *const @This()) bool {
        return self.value_type == .VAL_OBJ;
    }

    pub inline fn isFalse(self: *const @This()) bool {
        return self.value_type == .VAL_FALSE or self.value_type == .VAL_NULL;
    }

    pub inline fn isTrue(self: *const @This()) bool {
        return self.value_type == .VAL_TRUE;
    }

    pub inline fn isNull(self: *const @This()) bool {
        return self.value_type == .VAL_NULL;
    }

    pub inline fn isNum(self: *const @This()) bool {
        return self.value_type == .VAL_NUM;
    }

    pub inline fn isUndefined(self: *const @This()) bool {
        return self.value_type == .VAL_UNDEFINED;
    }
};

pub const ObjType = enum {
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FIBER,
    OBJ_FUNC,
    OBJ_FOREIGN,
    OBJ_INSTANCE,
    OBJ_LIST,
    OBJ_MAP,
    OBJ_MODULE,
    OBJ_RANGE,
    OBJ_STRING,
    OBJ_UPVALUE,
};

pub const Obj = struct {
    obj_type: ObjType,
    is_dark: bool,
    class_obj: *ObjClass,
    next: ?*Obj = undefined,

    pub fn hashCode(self: *@This()) u64 {
        switch (self.obj_type) {
            .OBJ_CLASS => return self.asClass().hashCode(),
            .OBJ_FUNC => return self.asFunc().hashCode(), // TODO 优化: 这个hash计算有些不合理
            .OBJ_RANGE => return self.asRange().hashCode(),
            .OBJ_STRING => return self.asString().hashCode(),
            else => @panic("Only immutable objects can be hashed."),
        }
        return 0;
    }

    pub fn eql(self: *@This(), other: *@This()) bool {
        if (self.obj_type != other.obj_type) return false;
        switch (self.obj_type) {
            .OBJ_RANGE => return self.asRange().eql(other.asRange()),
            .OBJ_STRING => return self.asString().eql(other.asString()),
            else => return self == other,
        }
    }

    pub inline fn toVal(self: *@This()) Value {
        return Value.fromObj(self);
    }

    pub fn dumpObj(self: *@This()) void {
        switch (self.obj_type) {
            .OBJ_CLASS => print("[class {s} {*}]", .{ self.asClass().name.value, self }),
            .OBJ_CLOSURE => print("[closure {*}]", .{self.asClosure()}),
            .OBJ_FIBER => print("[fiber {*}]", .{self.asFiber()}),
            .OBJ_FUNC => print("[fn {*}]", .{self.asFunc()}),
            .OBJ_FOREIGN => print("[foreign {*}]", .{self.asForeign()}),
            .OBJ_INSTANCE => print("[instance {*}]", .{self.asInstance()}),
            .OBJ_LIST => print("[list {*}]", .{self.asList()}),
            .OBJ_MAP => print("[map {*}]", .{self.asMap()}),
            .OBJ_MODULE => print("[module {*}]", .{self.asModule()}),
            .OBJ_RANGE => print("[range {*}]", .{self.asRange()}),
            .OBJ_STRING => print("{s}", .{self.asString().value}),
            .OBJ_UPVALUE => print("[upvalue {*}]", .{self.asUpvalue()}),
        }
    }

    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn as(self: *@This(), comptime T: type) *T {
        return @ptrCast(self);
    }

    pub inline fn asClass(self: *@This()) *ObjClass {
        return @ptrCast(self);
    }

    pub inline fn asClosure(self: *@This()) *ObjClosure {
        return @ptrCast(self);
    }

    pub inline fn asFiber(self: *@This()) *ObjFiber {
        return @ptrCast(self);
    }

    pub inline fn asFunc(self: *@This()) *ObjFunc {
        return @ptrCast(self);
    }

    pub inline fn asForeign(self: *@This()) *ObjForeign {
        return @ptrCast(self);
    }

    pub inline fn asInstance(self: *@This()) *ObjInstance {
        return @ptrCast(self);
    }

    pub inline fn asList(self: *@This()) *ObjList {
        return @ptrCast(self);
    }

    pub inline fn asMap(self: *@This()) *ObjMap {
        return @ptrCast(self);
    }

    pub inline fn asModule(self: *@This()) *ObjModule {
        return @ptrCast(self);
    }

    pub inline fn asRange(self: *@This()) *ObjRange {
        return @ptrCast(self);
    }

    pub inline fn asString(self: *@This()) *ObjString {
        return @ptrCast(self);
    }

    pub inline fn asUpvalue(self: *@This()) *ObjUpvalue {
        return @ptrCast(self);
    }

    pub inline fn isObjType(self: *const @This(), obj_type: ObjType) bool {
        return self.obj_type == obj_type;
    }
};

pub const ObjClass = struct {
    obj: Obj,
    super_class: ?*ObjClass,
    num_fields: ?usize, // 为null时表示为Foreign?
    methods: MethodBuffer,
    name: *ObjString,
    attributes: Value,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn hashCode(self: *@This()) u64 {
        return self.name.hash;
    }

    pub fn bindMethodCode(self: *@This(), func: *ObjFunc) void {
        var ip: usize = 0;

        while (true) {
            const instruction: OpCode = @enumFromInt(func.code.at(ip));
            switch (instruction) {
                // 将this类的字段向下移动到继承的字段之上
                // 这里不检查溢出，因为子类创建时会看到字段数量是否溢出.
                .CODE_LOAD_FIELD,
                .CODE_STORE_FIELD,
                .CODE_LOAD_FIELD_THIS,
                .CODE_STORE_FIELD_THIS,
                => func.code.rat(ip + 1).* += @intCast(self.super_class.?.num_fields.?),

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
                    // 向constant槽中填充对超类的引用.
                    const constant = Utils.b8Tob16(func.code.at(ip + 3), func.code.at(ip + 4));
                    func.constants.rat(constant).* = self.super_class.?.asObj().toVal();
                },

                .CODE_CLOSURE => {
                    // 绑定嵌套的闭包.
                    const constant = Utils.b8Tob16(func.code.at(ip + 1), func.code.at(ip + 2));
                    self.bindMethodCode(func.constants.at(constant).asFunc());
                    break;
                },

                .CODE_END => return,

                else => {},
            }
            ip += 1 + func.getByteCountForArguments(ip);
        }
    }
};

pub const ObjClosure = struct {
    obj: Obj,
    func: *ObjFunc,
    upvalues: []?*ObjUpvalue = undefined,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }
};

pub const ObjFiber = struct {
    obj: Obj,
    allocator: std.mem.Allocator,
    stack: GenericStack(Value) = undefined,
    frame: *CallFrame = undefined,
    frames: []CallFrame = undefined,
    raw_frames: GenericBuffer(CallFrame) = undefined,
    num_frames: usize = 0,
    open_upvalues: ?*ObjUpvalue = null,
    caller: ?*ObjFiber = null,
    err: Value = .NULL_VAL,
    state: FiberState = .FIBER_OTHER,

    pub fn init(allocator: std.mem.Allocator) ObjFiber {
        return .{ .allocator = allocator };
    }

    pub fn fromStack(self: *@This(), stack: GenericStack(Value)) void {
        self.stack = stack;
    }

    pub fn initStack(self: *@This(), capacity: usize) void {
        self.stack = GenericStack(Value).init(self.allocator);
        self.stack.resize(capacity) catch unreachable;
    }

    pub fn fromFrames(self: *@This(), frames: GenericBuffer(CallFrame)) void {
        self.raw_frames = frames;
        self.frames = self.raw_frames.data.items;
        self.num_frames = 0;
    }

    pub fn initFrames(self: *@This(), capacity: usize) void {
        self.raw_frames = GenericBuffer(CallFrame).init(self.allocator);
        self.raw_frames.resize(capacity);
        self.frames = self.raw_frames.data.items;
        self.num_frames = 0;
    }

    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn isDone(self: *@This()) bool {
        return self.num_frames == 0 or self.hasErr();
    }

    pub inline fn deinit(self: *@This()) void {
        self.stack.deinit();
        self.raw_frames.deinit();
    }

    pub inline fn push(self: *@This(), value: Value) void {
        self.stack.push(value);
    }

    pub inline fn pop(self: *@This()) *Value {
        return self.stack.pop();
    }

    pub inline fn drop(self: *@This()) void {
        self.stack.drop();
    }

    pub inline fn peek(self: *@This(), n: usize) *Value {
        return self.stack.peek(n);
    }

    pub inline fn peek1(self: *@This()) Value {
        return self.stack.peek1().*;
    }

    pub inline fn peek2(self: *@This()) Value {
        return self.stack.peek2().*;
    }

    pub inline fn loadStack(self: *@This(), i: usize) void {
        self.stack.top = i;
    }

    pub inline fn ensureFrameCapacity(self: *@This(), capacity: usize) void {
        if (capacity > self.raw_frames.len()) {
            self.raw_frames.resize(self.raw_frames.len() * 2);
            self.frames = self.raw_frames.data.items;
        }
    }

    // 将闭包推入栈中，并创建一个帧. (但并未执行)
    pub fn pushFrame(self: *@This(), closure: *ObjClosure, offsetToStack: usize) void {
        self.ensureFrameCapacity(self.num_frames + 1);
        // 调用帧+1, 起始栈设置为传入的栈, 闭包设置为传入的闭包, 指令指向闭包的fn指令.
        var frame = &self.frames[self.num_frames];
        self.num_frames += 1;

        frame.setStack(self.stack.buffer, offsetToStack);
        frame.closure = closure;
        frame.ip = closure.func.code.data.items; // TODO 改进ip为对象?
        frame.ip_offset = 0;
    }

    pub inline fn saveFrame(self: *@This()) void {
        _ = self;
    }

    pub inline fn loadFrame(self: *@This()) void {
        self.frame = &self.frames[self.num_frames - 1];
    }

    pub inline fn dropFrame(self: *@This()) void {
        self.num_frames -= 1;
    }

    pub inline fn hasErr(self: *@This()) bool {
        return !self.err.isNull();
    }

    pub fn remapFrames(self: *@This()) void {
        for (0..self.num_frames) |i| {
            self.frames[i].setStackRef(self.stack.buffer);
        }
    }

    pub fn remapUpvalues(self: *@This()) void {
        var upvalue: ?*ObjUpvalue = self.open_upvalues;
        while (upvalue) |up| : (upvalue = up.next) {
            up.value = &self.stack.buffer[up.from_stack_index];
        }
    }

    // 闭环任何[last]及以上为栈槽创建的upvalue.
    pub fn closeUpvalues(self: *@This(), last: *Value) void {
        while (self.open_upvalues) |upvalue| {
            // TODO 这里需要改进
            if (@intFromPtr(upvalue.*.value) < @intFromPtr(last)) break;
            upvalue.close();
            self.open_upvalues = upvalue.next; // 从upvalue列表移除.
        }
    }

    pub fn dumpStack(self: *@This()) void {
        print("(fiber {*}) ", .{self});
        for (0..self.stack.top) |i| {
            self.stack.buffer[i].dumpVal();
            print(" | ", .{});
        }
        print("\n", .{});
    }
};

pub const ObjFunc = struct {
    obj: Obj,
    code: ByteBuffer,
    constants: ValueBuffer,
    module: ?*ObjModule,
    max_slots: usize,
    num_upvalues: usize,
    arity: u64,
    debug: *FuncDebug,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn checkArity(self: *@This(), numArgs: u8) bool {
        return numArgs - 1 >= self.arity;
    }

    pub inline fn hashCode(self: *@This()) u64 {
        return Utils.hashNum(self.arity) ^ Utils.hashNum(self.code.count);
    }

    // 返回[fn]字节码中[ip]处的指令的参数的字节数.
    pub fn getByteCountForArguments(self: *@This(), ip: usize) usize {
        const instruction: OpCode = @enumFromInt(self.code.at(ip));
        switch (instruction) {
            .CODE_NULL,
            .CODE_FALSE,
            .CODE_TRUE,
            .CODE_POP,
            .CODE_CLOSE_UPVALUE,
            .CODE_RETURN,
            .CODE_END,
            .CODE_LOAD_LOCAL_0,
            .CODE_LOAD_LOCAL_1,
            .CODE_LOAD_LOCAL_2,
            .CODE_LOAD_LOCAL_3,
            .CODE_LOAD_LOCAL_4,
            .CODE_LOAD_LOCAL_5,
            .CODE_LOAD_LOCAL_6,
            .CODE_LOAD_LOCAL_7,
            .CODE_LOAD_LOCAL_8,
            .CODE_CONSTRUCT,
            .CODE_FOREIGN_CONSTRUCT,
            .CODE_FOREIGN_CLASS,
            .CODE_END_MODULE,
            .CODE_END_CLASS,
            => return 0,

            .CODE_LOAD_LOCAL,
            .CODE_STORE_LOCAL,
            .CODE_LOAD_UPVALUE,
            .CODE_STORE_UPVALUE,
            .CODE_LOAD_FIELD_THIS,
            .CODE_STORE_FIELD_THIS,
            .CODE_LOAD_FIELD,
            .CODE_STORE_FIELD,
            .CODE_CLASS,
            => return 1,

            .CODE_CONSTANT,
            .CODE_LOAD_MODULE_VAR,
            .CODE_STORE_MODULE_VAR,
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
            .CODE_JUMP,
            .CODE_LOOP,
            .CODE_JUMP_IF,
            .CODE_AND,
            .CODE_OR,
            .CODE_METHOD_INSTANCE,
            .CODE_METHOD_STATIC,
            .CODE_IMPORT_MODULE,
            .CODE_IMPORT_VARIABLE,
            => return 2,

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
            => return 4,

            .CODE_CLOSURE => {
                const constant = Utils.b8Tob16(self.code.at(ip + 1), self.code.at(ip + 2));
                const loadedFn = self.constants.at(constant).asFunc();
                return 2 + loadedFn.num_upvalues * 2; // 对于constant来说是两个字节, 每个upvalue是两个字节.
            },
        }
        @panic("Unreachable!");
    }
};

pub const ObjForeign = struct {
    obj: Obj,
    data: [*]u8 = undefined,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn cast(self: *@This(), comptime T: type) *T {
        return @ptrCast(@alignCast(self.data));
    }
};

pub const ObjInstance = struct {
    obj: Obj,
    fields: []Value,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }
};

pub const ObjList = struct {
    obj: Obj,
    elements: ValueBuffer,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn allocatedSize(self: *const @This()) usize {
        return self.elements.allocatedSize();
    }
};

pub const ObjMap = struct {
    obj: Obj = undefined,
    entries: ValueHashMap,

    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn mapGet(self: *@This(), key: Value) Value {
        return if (self.entries.get(key)) |entry| entry else .UNDEFINED_VAL;
    }

    pub inline fn mapContains(self: *@This(), key: Value) bool {
        return self.entries.contains(key);
    }

    pub inline fn mapCount(self: *@This()) usize {
        return self.entries.count();
    }

    pub inline fn mapSet(self: *@This(), key: Value, value: Value) void {
        self.entries.put(key, value) catch unreachable;
    }

    pub inline fn removeKey(self: *@This(), key: Value) bool {
        return self.entries.remove(key);
    }

    pub inline fn count(self: *const @This()) usize {
        return self.entries.count();
    }

    pub inline fn capacity(self: *const @This()) usize {
        return self.entries.capacity();
    }

    pub inline fn iterator(self: *const @This()) ValueHashMap.Iterator {
        return self.entries.iterator();
    }

    pub fn allocatedSize(self: *const @This()) usize {
        // TODO 此 部分和zig版本相关, 需要改进
        const unmanagedType = @TypeOf(self.entries.unmanaged);
        const KV = unmanagedType.KV;
        const K = @FieldType(KV, "key");
        const V = @FieldType(KV, "value");
        const Header = struct {
            values: [*]V,
            keys: [*]K,
            capacity: unmanagedType.Size,
        };
        const Metadata = u8;

        const header_align = @alignOf(Header);
        const key_align = if (@sizeOf(K) == 0) 1 else @alignOf(K);
        const val_align = if (@sizeOf(V) == 0) 1 else @alignOf(V);
        const max_align = comptime @max(header_align, key_align, val_align);

        const cap: usize = self.capacity();
        const meta_size = @sizeOf(Header) + cap * @sizeOf(Metadata);
        comptime std.debug.assert(@alignOf(Metadata) == 1);

        const keys_start = std.mem.alignForward(usize, meta_size, key_align);
        const keys_end = keys_start + cap * @sizeOf(K);

        const vals_start = std.mem.alignForward(usize, keys_end, val_align);
        const vals_end = vals_start + cap * @sizeOf(V);

        const total_size = std.mem.alignForward(usize, vals_end, max_align);
        return total_size;
    }

    pub inline fn clear(self: *@This()) void {
        self.entries.clearAndFree();
    }
};

pub const ObjModule = struct {
    obj: Obj,
    variables: ValueBuffer,
    variable_names: SymbolTable,
    name: ?*ObjString = null,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }
};

pub const ObjRange = struct {
    obj: Obj,
    from: f64,
    to: f64,
    is_inclusive: bool, // 为true: 包含to, false: 开区间, 不包含to
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn eql(self: *@This(), other: *ObjRange) bool {
        return self.from == other.from and self.to == other.to and self.is_inclusive == other.is_inclusive;
    }

    pub inline fn hashCode(self: *@This()) u64 {
        return Utils.hashNum(self.from) ^ Utils.hashNum(self.to);
    }
};

pub const ObjString = struct {
    obj: Obj,
    hash: u64,
    value: []const u8,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    pub inline fn eql(self: *@This(), other: *ObjString) bool {
        return self.hash == other.hash and std.mem.eql(u8, self.value, other.value);
    }

    pub inline fn doHash(self: *@This()) void {
        self.hash = Utils.hashStr(self.value);
    }

    pub inline fn hashCode(self: *@This()) u64 {
        return self.hash;
    }

    pub inline fn strFind(self: *@This(), needle: *ObjString, start: usize) ?usize {
        return std.mem.indexOf(u8, self.value[start..], needle.value);
    }

    pub inline fn strContains(self: *@This(), needle: *ObjString, start: usize) bool {
        return self.strFind(needle, start) != null;
    }

    pub inline fn startsWith(self: *@This(), prefix: *ObjString) bool {
        return std.mem.startsWith(u8, self.value, prefix.value);
    }

    pub inline fn endsWith(self: *@This(), suffix: *ObjString) bool {
        return std.mem.endsWith(u8, self.value, suffix.value);
    }
};

pub const ObjUpvalue = struct {
    obj: Obj,
    from_stack_index: usize,
    value: *Value,
    closed: Value,
    next: ?*ObjUpvalue = null,
    pub inline fn asObj(self: *@This()) *Obj {
        return @ptrCast(self);
    }

    // 封闭upvalue
    pub inline fn close(self: *@This()) void {
        self.closed = self.value.*; // 拷贝
        self.value = &self.closed; // 指向自身
    }
};
