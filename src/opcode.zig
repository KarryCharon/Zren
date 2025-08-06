pub const OpCode = enum(u8) {
    CODE_CONSTANT, //     加载常量到给定的 index [arg]
    CODE_NULL, //         将 null 到stack
    CODE_FALSE, //        将 false 到stack
    CODE_TRUE, //         将 true 到stack

    CODE_LOAD_LOCAL_0, // 将 value push到给定的local槽
    CODE_LOAD_LOCAL_1,
    CODE_LOAD_LOCAL_2,
    CODE_LOAD_LOCAL_3,
    CODE_LOAD_LOCAL_4,
    CODE_LOAD_LOCAL_5,
    CODE_LOAD_LOCAL_6,
    CODE_LOAD_LOCAL_7,
    CODE_LOAD_LOCAL_8,
    // 注意: 编译器假定以下 _STORE 指令始终紧跟其对应的 _LOAD 指令之后
    CODE_LOAD_LOCAL, // 将 value 加载到给定的local槽
    CODE_STORE_LOCAL, // 存储stack顶部的值到给定的local槽[arg], 不会pop
    CODE_LOAD_UPVALUE, // 将当前帧的closure的upvalue[arg]的value推到当前帧栈顶
    CODE_STORE_UPVALUE, // 将当前帧栈顶的值存储到到当前帧的closure的upvalue [arg]
    CODE_LOAD_MODULE_VAR, // 将模块的 top-level(顶层) slot [arg] 值推送到当前帧栈顶
    CODE_STORE_MODULE_VAR, // 将当前帧栈顶的值到 top-level 的变量槽 slot [arg]
    // 将字段的 value push到当前函数的receiver的 slot [arg]. 这被用于在方法中直接访问this的常规字段
    CODE_LOAD_FIELD_THIS,
    // 存储栈顶到current value的receiver的 field slot [arg]. 不会pop. 该指令比通用的CODE_LOAD_FIELD更快
    CODE_STORE_FIELD_THIS,
    CODE_LOAD_FIELD, // 弹出一个instance, 然后将它的字段值push到 slot[arg]
    CODE_STORE_FIELD, // 弹出一个instance, 然后后续栈顶的内容存储到它的字段slot[arg]中. 不会pop
    CODE_POP, // 弹出并丢弃stack顶部的值
    // 使用 symbol[arg] 调用方法. 后缀数字表示参数的数量(不包含receiver)
    CODE_CALL_0,
    CODE_CALL_1,
    CODE_CALL_2,
    CODE_CALL_3,
    CODE_CALL_4,
    CODE_CALL_5,
    CODE_CALL_6,
    CODE_CALL_7,
    CODE_CALL_8,
    CODE_CALL_9,
    CODE_CALL_10,
    CODE_CALL_11,
    CODE_CALL_12,
    CODE_CALL_13,
    CODE_CALL_14,
    CODE_CALL_15,
    CODE_CALL_16,

    // 使用 symbol[arg] 调用superclass的方法. 后缀数字表示参数的数量(不包含receiver)
    CODE_SUPER_0,
    CODE_SUPER_1,
    CODE_SUPER_2,
    CODE_SUPER_3,
    CODE_SUPER_4,
    CODE_SUPER_5,
    CODE_SUPER_6,
    CODE_SUPER_7,
    CODE_SUPER_8,
    CODE_SUPER_9,
    CODE_SUPER_10,
    CODE_SUPER_11,
    CODE_SUPER_12,
    CODE_SUPER_13,
    CODE_SUPER_14,
    CODE_SUPER_15,
    CODE_SUPER_16,

    CODE_JUMP, // 向前跳转 指令pointer[arg]
    CODE_LOOP, // 向后跳转 指令pointer[arg]
    CODE_JUMP_IF, // 弹出, 如果不为真则向前跳转 指令pointer[arg]
    CODE_AND, // 栈顶是false则向前转跳[arg]. 否则弹出并继续执行
    CODE_OR, // 如果栈顶非false则向前跳转[arg]. 否则弹出并继续执行
    CODE_CLOSE_UPVALUE, // 闭环栈顶的local upvalue, 并且弹出它
    CODE_RETURN, // 从当前函数退出, 并返回栈顶的值

    // 为常量表中 [arg] 处存储的函数创建一个闭包
    //
    // 函数参数后面是一堆参数, 每个upvalue有两个.
    // 当被捕获的变量是局部变量(而不是upvalue)时第一个值为true, 第二个值是捕获的局部变量或upvalue的索引.
    //
    CODE_CLOSURE, // 推送创建的闭包

    // 构造一个class的实例
    //
    // 假设class object在slot的0位置, 并将其替换为该class的新的未初始化的实例.
    // 该opcode仅由被编译器生成的构造器的metaclass方法触发.
    CODE_CONSTRUCT,

    // 构造一个foreign class的实例
    //
    // 假设class object在slot的0位置, 并将其替换为该class的新的未初始化的实例.
    // 该opcode仅由被编译器生成的构造器的metaclass方法触发.
    CODE_FOREIGN_CONSTRUCT,

    CODE_CLASS, // 创建一个class. 栈顶是superclass. 其后是class的字符串name. Byte[arg]是class的字段数量.
    CODE_END_CLASS, // 类的的终止标记. 栈中包含class和ClassAttributes(可为null).
    CODE_FOREIGN_CLASS, // 创建一个foraign class. 栈顶是superclass. 其后是class的字符串name. Byte[arg]是class的字段数量.

    // 为symbol[arg]定义一个方法. 接收该方法的class从栈中弹出, 然后定义方法体的函数被弹出.
    //
    // 如果定义了一个foreign方法, 则"function"是一个标识foreign方法的字符串, 否则它是一个函数或闭包.
    CODE_METHOD_INSTANCE,

    // 为symbol[arg]定义一个方法. 其metaclass会接收该method的class会被弹出, 然后定义方法体的函数被弹出.
    //
    // 如果定义了一个foreign方法, 则"function"是一个标识foreign方法的字符串, 否则它是一个函数或闭包.
    CODE_METHOD_STATIC,

    // 这会在module体结束时执行. push null 到栈顶作为import 语句的返回值, 并将module存储为最近导入的模块.
    CODE_END_MODULE,

    // 导入一个模块, name为存储在常量表[arg]处的字符串
    //
    // 将 null push到栈中, 以便导入模块的fiber返回时可以将其替换为虚拟值.
    // 在fibers恢复caller时会返回一个value.
    CODE_IMPORT_MODULE,
    // 从最近导入的模块中导入一个变量. name为存储在常量表[arg]处的字符串.
    // 将 导入的变量值 push到栈中.
    CODE_IMPORT_VARIABLE,
    // 此伪指令表示字节码的结束。它应该始终以 CODE_RETURN 开头，因此实际上永远不会执行。
    CODE_END,

    pub fn load_local_n(n: u8) @This() {
        return OpCode.CODE_LOAD_LOCAL_0.poffset(n);
    }

    pub inline fn poffset(self: @This(), offset: u8) @This() {
        return @enumFromInt(@intFromEnum(self) + offset);
    }

    pub inline fn noffset(self: @This(), offset: u8) @This() {
        return @enumFromInt(@intFromEnum(self) - offset);
    }

    pub inline fn num(self: @This()) u8 {
        return @intFromEnum(self);
    }
};
