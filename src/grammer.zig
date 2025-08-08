const std = @import("std");
const Compiler = @import("parser.zig").Compiler;
const Constant = @import("constants.zig");
const Utils = @import("utils.zig");
const GrammarFn = *const fn (*Compiler, bool) void;
const SignatureFn = *const fn (*Compiler, *Signature) void;

pub const Precedence = enum(u8) {
    PREC_NONE,
    PREC_LOWEST,
    PREC_ASSIGNMENT, //      =
    PREC_CONDITIONAL, //     ?:
    PREC_LOGICAL_OR, //      ||
    PREC_LOGICAL_AND, //     &&
    PREC_EQUALITY, //        == !=
    PREC_IS, //              is
    PREC_COMPARISON, //      < > <= >=
    PREC_BITWISE_OR, //      |
    PREC_BITWISE_XOR, //     ^
    PREC_BITWISE_AND, //     &
    PREC_BITWISE_SHIFT, //   << >>
    PREC_RANGE, //           .. ...
    PREC_TERM, //            + -
    PREC_FACTOR, //          * / %
    PREC_UNARY, //           unary - ! ~
    PREC_CALL, //            . () []
    PREC_PRIMARY,

    pub inline fn num(self: @This()) u8 {
        return @intFromEnum(self);
    }

    pub inline fn poffset(self: @This(), offset: u8) @This() {
        return @enumFromInt(@intFromEnum(self) + offset);
    }

    pub inline fn noffset(self: @This(), offset: u8) @This() {
        return @enumFromInt(@intFromEnum(self) - offset);
    }
};

// 不同类型方法的签名语法
pub const SignatureType = enum {
    SIG_METHOD, //           一个名字, 后面跟着一个(可能为空的)括号参数列表, 也用于二元操作符
    SIG_GETTER, //           只有名字, 也用于一元操作符
    SIG_SETTER, //           一个名字, 后面跟一个"="
    SIG_SUBSCRIPT, //        一个方括号, 包含了参数列表
    SIG_SUBSCRIPT_SETTER, // 一个方括号, 包含了参数列表, 后面跟一个"="
    SIG_INITIALIZER, //      构造器的初始化函数. 有不同的签名, 已防其在metaclass的构造器外被直接调用.
};

pub const Signature = struct {
    name: []const u8 = "",
    stype: SignatureType = undefined,
    arity: u8 = 0,

    pub fn toString(self: *@This(), name: []u8) []const u8 {
        var length: usize = 0;
        std.mem.copyForwards(u8, name[length..], self.name);
        length += self.name.len;

        switch (self.stype) {
            .SIG_METHOD => length += self.parameterList(name[length..], self.arity, '(', ')'),

            .SIG_GETTER => {}, // 其签名仅有一个名字

            .SIG_SETTER => {
                name[length] = '=';
                length += 1;
                length += self.parameterList(name[length..], 1, '(', ')');
            },

            .SIG_SUBSCRIPT => length += self.parameterList(name[length..], self.arity, '[', ']'),

            .SIG_SUBSCRIPT_SETTER => {
                length += self.parameterList(name[length..], self.arity - 1, '[', ']');
                name[length] = '=';
                length += 1;
                length += self.parameterList(name[length..], 1, '(', ')');
            },

            .SIG_INITIALIZER => {
                std.mem.copyForwards(u8, name, "init ");
                std.mem.copyForwards(u8, name[5..], self.name);
                length = self.name.len + 5;
                length += self.parameterList(name[length..], self.arity, '(', ')');
            },
        }
        name[length] = '\x00';
        return name[0..length];
    }

    // 向[name]中追加[num_params]个"_"字符, 并用[left_bracket]和[right_bracket]包裹起来.
    pub fn parameterList(self: *@This(), name: []u8, num_params: usize, left_bracket: u8, right_bracket: u8) usize {
        _ = self;
        var length: usize = 0;
        name[length] = left_bracket;
        length += 1;
        for (0..num_params) |i| {
            if (i >= Constant.C.MAX_PARAMETERS) break;

            if (i > 0) {
                name[length] = ',';
                length += 1;
            }

            name[length] = '_';
            length += 1;
        }
        name[length] = right_bracket;
        length += 1;
        return length;
    }
};

pub const GrammarRule = struct {
    prefix: ?GrammarFn,
    infix: ?GrammarFn,
    method: ?SignatureFn,
    precedence: Precedence,
    name: []const u8,
    pub fn init(
        prefix: ?GrammarFn,
        infix: ?GrammarFn,
        method: ?SignatureFn,
        precedence: Precedence,
        name: []const u8,
    ) @This() {
        return .{
            .prefix = prefix,
            .infix = infix,
            .method = method,
            .precedence = precedence,
            .name = name,
        };
    }

    pub fn UNUSED() @This() {
        return G.init(null, null, null, .PREC_NONE, "");
    }

    pub fn PREFIX(f: ?GrammarFn) @This() {
        return G.init(f, null, null, .PREC_NONE, "");
    }

    pub fn PREFIX_OPERATOR(name: []const u8) @This() {
        return G.init(unaryOp, null, unarySignature, .PREC_NONE, name);
    }

    pub fn INFIX(prec: Precedence, f: ?GrammarFn) @This() {
        return G.init(null, f, null, prec, "");
    }

    pub fn INFIX_OPERATOR(prec: Precedence, name: []const u8) @This() {
        return G.init(null, infixOp, infixSignature, prec, name);
    }

    pub fn OPERATOR(name: []const u8) @This() {
        return G.init(unaryOp, infixOp, mixedSignature, .PREC_TERM, name);
    }
};

const G = GrammarRule;

pub const rules: []const G = &.{
    G.PREFIX(grouping), //                                          /* TOKEN_LEFT_PAREN    */
    G.UNUSED(), //                                                  /* TOKEN_RIGHT_PAREN   */
    G.init(list, subscript, subscriptSignature, .PREC_CALL, ""), // /* TOKEN_LEFT_BRACKET  */
    G.UNUSED(), //                                                  /* TOKEN_RIGHT_BRACKET */
    G.PREFIX(map), //                                               /* TOKEN_LEFT_BRACE    */
    G.UNUSED(), //                                                  /* TOKEN_RIGHT_BRACE   */
    G.UNUSED(), //                                                  /* TOKEN_COLON         */
    G.INFIX(.PREC_CALL, call), //                                   /* TOKEN_DOT           */
    G.INFIX_OPERATOR(.PREC_RANGE, ".."), //                         /* TOKEN_DOTDOT        */
    G.INFIX_OPERATOR(.PREC_RANGE, "..."), //                        /* TOKEN_DOTDOTDOT     */
    G.UNUSED(), //                                                  /* TOKEN_COMMA         */
    G.INFIX_OPERATOR(.PREC_FACTOR, "*"), //                         /* TOKEN_STAR          */
    G.INFIX_OPERATOR(.PREC_FACTOR, "/"), //                         /* TOKEN_SLASH         */
    G.INFIX_OPERATOR(.PREC_FACTOR, "%"), //                         /* TOKEN_PERCENT       */
    G.UNUSED(), //                                                  /* TOKEN_HASH          */
    G.INFIX_OPERATOR(.PREC_TERM, "+"), //                           /* TOKEN_PLUS          */
    G.OPERATOR("-"), //                                             /* TOKEN_MINUS         */
    G.INFIX_OPERATOR(.PREC_BITWISE_SHIFT, "<<"), //                 /* TOKEN_LTLT          */
    G.INFIX_OPERATOR(.PREC_BITWISE_SHIFT, ">>"), //                 /* TOKEN_GTGT          */
    G.INFIX_OPERATOR(.PREC_BITWISE_OR, "|"), //                     /* TOKEN_PIPE          */
    G.INFIX(.PREC_LOGICAL_OR, or_), //                              /* TOKEN_PIPEPIPE      */
    G.INFIX_OPERATOR(.PREC_BITWISE_XOR, "^"), //                    /* TOKEN_CARET         */
    G.INFIX_OPERATOR(.PREC_BITWISE_AND, "&"), //                    /* TOKEN_AMP           */
    G.INFIX(.PREC_LOGICAL_AND, and_), //                            /* TOKEN_AMPAMP        */
    G.PREFIX_OPERATOR("!"), //                                      /* TOKEN_BANG          */
    G.PREFIX_OPERATOR("~"), //                                      /* TOKEN_TILDE         */
    G.INFIX(.PREC_ASSIGNMENT, conditional), //                      /* TOKEN_QUESTION      */
    G.UNUSED(), //                                                  /* TOKEN_EQ            */
    G.INFIX_OPERATOR(.PREC_COMPARISON, "<"), //                     /* TOKEN_LT            */
    G.INFIX_OPERATOR(.PREC_COMPARISON, ">"), //                     /* TOKEN_GT            */
    G.INFIX_OPERATOR(.PREC_COMPARISON, "<="), //                    /* TOKEN_LTEQ          */
    G.INFIX_OPERATOR(.PREC_COMPARISON, ">="), //                    /* TOKEN_GTEQ          */
    G.INFIX_OPERATOR(.PREC_EQUALITY, "=="), //                      /* TOKEN_EQEQ          */
    G.INFIX_OPERATOR(.PREC_EQUALITY, "!="), //                      /* TOKEN_BANGEQ        */
    G.UNUSED(), //                                                  /* TOKEN_BREAK         */
    G.UNUSED(), //                                                  /* TOKEN_CONTINUE      */
    G.UNUSED(), //                                                  /* TOKEN_CLASS         */
    G.init(null, null, constructorSignature, .PREC_NONE, ""), //    /* TOKEN_CONSTRUCT     */
    G.UNUSED(), //                                                  /* TOKEN_ELSE          */
    G.PREFIX(boolean), //                                           /* TOKEN_FALSE         */
    G.UNUSED(), //                                                  /* TOKEN_FOR           */
    G.UNUSED(), //                                                  /* TOKEN_FOREIGN       */
    G.UNUSED(), //                                                  /* TOKEN_IF            */
    G.UNUSED(), //                                                  /* TOKEN_IMPORT        */
    G.UNUSED(), //                                                  /* TOKEN_AS            */
    G.UNUSED(), //                                                  /* TOKEN_IN            */
    G.INFIX_OPERATOR(.PREC_IS, "is"), //                            /* TOKEN_IS            */
    G.PREFIX(null_), //                                             /* TOKEN_NULL          */
    G.UNUSED(), //                                                  /* TOKEN_RETURN        */
    G.UNUSED(), //                                                  /* TOKEN_STATIC        */
    G.PREFIX(super_), //                                            /* TOKEN_SUPER         */
    G.PREFIX(this_), //                                             /* TOKEN_THIS          */
    G.PREFIX(boolean), //                                           /* TOKEN_TRUE          */
    G.UNUSED(), //                                                  /* TOKEN_VAR           */
    G.UNUSED(), //                                                  /* TOKEN_WHILE         */
    G.PREFIX(field), //                                             /* TOKEN_FIELD         */
    G.PREFIX(staticField), //                                       /* TOKEN_STATIC_FIELD  */
    G.init(name_, null, namedSignature, .PREC_NONE, ""), //         /* TOKEN_NAME          */
    G.PREFIX(literal), //                                           /* TOKEN_NUMBER        */
    G.PREFIX(literal), //                                           /* TOKEN_STRING        */
    G.PREFIX(stringInterpolation), //                               /* TOKEN_INTERPOLATION */
    G.UNUSED(), //                                                  /* TOKEN_LINE          */
    G.UNUSED(), //                                                  /* TOKEN_ERROR         */
    G.UNUSED(), //                                                  /* TOKEN_EOF           */
};

// ----------------------------------------------------------------------------------------

// 括号表达式.
fn grouping(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    compiler.expression();
    compiler.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

// 列表字面量
fn list(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;

    // 初始化新列表
    compiler.loadCoreVariable("List");
    compiler.callMethod(0, "new()");

    // 编译列表元素.每个元素的添加都编译成 ".add()" 调用.
    while (true) {
        compiler.ignoreNewLines();
        if (compiler.peek() == .TOKEN_RIGHT_BRACKET) break; // 当匹配到列表末尾时终止
        // 添加元素
        compiler.expression();
        compiler.callMethod(1, "addCore_(_)");
        if (!compiler.match(.TOKEN_COMMA)) break;
    }

    // 允许在']'前的空行.
    compiler.ignoreNewLines();
    compiler.consume(.TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.");
}

// 字典字面量
fn map(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    // 初始化新字典
    compiler.loadCoreVariable("Map");
    compiler.callMethod(0, "new()");

    // 编译字典元素. 每一个都被编译成下标setter.
    while (true) {
        compiler.ignoreNewLines();
        if (compiler.peek() == .TOKEN_RIGHT_BRACE) break; // 当匹配到字典末尾时终止

        // 字典键.
        compiler.parsePrecedence(.PREC_UNARY);
        compiler.consume(.TOKEN_COLON, "Expect ':' after map key.");
        compiler.ignoreNewLines();

        // 字典值.
        compiler.expression();
        compiler.callMethod(2, "addCore_(_,_)");
        if (!compiler.match(.TOKEN_COMMA)) break;
    }

    // 允许在'}'前的空行.
    compiler.ignoreNewLines();
    compiler.consume(.TOKEN_RIGHT_BRACE, "Expect '}}' after map entries.");
}

// 一元操作符, 如 `-foo`.
fn unaryOp(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    const rule = &rules[compiler.parser.prev.token_type.num()];
    compiler.ignoreNewLines();
    compiler.parsePrecedence(Precedence.PREC_UNARY.poffset(1)); // 编译参数.
    compiler.callMethod(0, rule.name); // 对左侧调用操作符方法.
}

fn boolean(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    compiler.emitOp(if (compiler.parser.prev.token_type == .TOKEN_FALSE) .CODE_FALSE else .CODE_TRUE);
}

fn field(compiler: *Compiler, canAssign: bool) void {
    // 用假值初始化它, 这样就可以继续解析并尽量减少级联错误的数量.
    var f: usize = Constant.C.MAX_FIELDS; // TODO 这个 f 需要改进(类型)

    var enclosing_class = compiler.getEnclosingClass();

    if (enclosing_class == null) {
        compiler.doError("Cannot reference a field outside of a class definition.", .{});
    } else if (enclosing_class.?.is_foreign) {
        compiler.doError("Cannot define fields in a foreign class.", .{});
    } else if (enclosing_class.?.in_static) {
        compiler.doError("Cannot use an instance field in a static method.", .{});
    } else {
        // 查找字段, 或隐式定义它.
        f = compiler.parser.vm.symbolTableEnsure(&enclosing_class.?.fields, compiler.parser.prev.name());

        if (f >= Constant.C.MAX_FIELDS) {
            compiler.doError("A class can only have {d} fields.", .{Constant.C.MAX_FIELDS});
        }
    }

    // 如果字段名后跟了一个 "=" 则是赋值操作.
    var isLoad = true;
    if (canAssign and compiler.match(.TOKEN_EQ)) {
        // 编译右侧.
        compiler.expression();
        isLoad = false;
    }

    // 如果直接在方法内部, 则使用更优化的指令.
    if (compiler.parent != null and compiler.parent.?.enclosing_class == enclosing_class) {
        _ = compiler.emitByteArg(if (isLoad) .CODE_LOAD_FIELD_THIS else .CODE_STORE_FIELD_THIS, @truncate(f));
    } else {
        compiler.loadThis();
        _ = compiler.emitByteArg(if (isLoad) .CODE_LOAD_FIELD else .CODE_STORE_FIELD, @truncate(f));
    }

    compiler.allowLineBeforeDot();
}

fn staticField(compiler: *Compiler, canAssign: bool) void {
    var classCompiler = compiler.getEnclosingClassCompiler() orelse {
        compiler.doError("Cannot use a static field outside of a class definition.", .{});
        return;
    };

    // 在作用域链上查找名称.
    const token = &compiler.parser.prev;

    // 如果是首次看到这个静态字段, 隐式定义它为类定义作用域中的变量.
    if (classCompiler.resolveLocal(token.name()) == null) {
        const symbol = classCompiler.declareVariable(null);

        // 隐式初始化为空.
        classCompiler.emitOp(.CODE_NULL);
        classCompiler.defineVariable(symbol);
    }

    // 到这里, 它肯定存在了, 所以能正确解析.
    // 这里和上面的 resolveLocal() 调用不同, 因为它可能已经被闭包捕获了.
    const variable = compiler.resolveName(token.name());
    compiler.bareName(canAssign, variable);
}

// 将变量名或方法调用与隐式接收者一起编译.
fn name_(compiler: *Compiler, canAssign: bool) void {
    compiler.parseName(canAssign);
}

fn null_(compiler: *Compiler, canAssign: bool) void {
    compiler.null_(canAssign);
}

// 数字或字符串字面量.
fn literal(compiler: *Compiler, canAssign: bool) void {
    compiler.literal(canAssign);
}

// 包含插值表达式的字符串字面量.
// 插值表达式是语法糖, 等价 ".join()" 方法.
// 因此, 字符串:  "a %(b + c) d"
// 会被编译成:    ["a ", b + c, " d"].join()
fn stringInterpolation(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    // 初始化新列表
    compiler.loadCoreVariable("List");
    compiler.callMethod(0, "new()");

    while (true) {
        // 字符串开头部分
        compiler.literal(false);
        compiler.callMethod(1, "addCore_(_)");

        // 插值表达式.
        compiler.ignoreNewLines();
        compiler.expression();
        compiler.callMethod(1, "addCore_(_)");

        compiler.ignoreNewLines();
        if (!compiler.match(.TOKEN_INTERPOLATION)) break;
    }

    // 字符串结尾.
    compiler.consume(.TOKEN_STRING, "Expect end of string interpolation.");
    compiler.literal(false);
    compiler.callMethod(1, "addCore_(_)");

    // 插值列表合并.
    compiler.callMethod(0, "join()");
}

fn super_(compiler: *Compiler, canAssign: bool) void {
    const enclosing_class = compiler.getEnclosingClass();
    if (enclosing_class == null) {
        compiler.doError("Cannot use 'super' outside of a method.", .{});
    }

    compiler.loadThis();

    // TODO Super operator calls.
    // TODO There's no syntax for invoking a superclass constructor with a different name from the enclosing one. Figure that out.

    // 检查是否是已命名的super call, 或者是未命名的调用.
    if (compiler.match(.TOKEN_DOT)) {
        // 编译 superclass call.
        compiler.consume(.TOKEN_NAME, "Expect method name after 'super.'.");
        compiler.namedCall(canAssign, .CODE_SUPER_0);
    } else if (enclosing_class) |e| {
        // 非显式名称, 所以使用封闭方法的名称.
        // 首先, 确保我们检查 enclosing_class 不是 NULL, 虽然已经报告了错误, 但这里不能崩溃.
        compiler.methodCall(.CODE_SUPER_0, e.signature);
    }
}

fn this_(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    if (compiler.getEnclosingClass() == null) {
        compiler.doError("Cannot use 'this' outside of a method.", .{});
        return;
    }

    compiler.loadThis();
}

fn subscript(compiler: *Compiler, canAssign: bool) void {
    var signature: Signature = .{
        .stype = .SIG_SUBSCRIPT,
        .arity = 0,
    };

    compiler.finishArgumentList(&signature);
    compiler.consume(.TOKEN_RIGHT_BRACKET, "Expect ']' after arguments.");

    compiler.allowLineBeforeDot();

    if (canAssign and compiler.match(.TOKEN_EQ)) {
        signature.stype = .SIG_SUBSCRIPT_SETTER;
        signature.arity += 1;
        compiler.validateNumParameters(signature.arity);
        compiler.expression();
    }

    compiler.callSignature(.CODE_CALL_0, &signature);
}

fn call(compiler: *Compiler, canAssign: bool) void {
    compiler.ignoreNewLines();
    compiler.consume(.TOKEN_NAME, "Expect method name after '.'.");
    compiler.namedCall(canAssign, .CODE_CALL_0);
}

fn and_(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    compiler.ignoreNewLines();

    // 如果左操作数为false, 则跳过右操作参数.
    const jump = compiler.emitJump(.CODE_AND);
    compiler.parsePrecedence(.PREC_LOGICAL_AND);
    compiler.patchJump(jump);
}

fn or_(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    compiler.ignoreNewLines();

    // 当左操作数为true时, 跳过右操作数.
    const jump = compiler.emitJump(.CODE_OR);
    compiler.parsePrecedence(.PREC_LOGICAL_OR);
    compiler.patchJump(jump);
}

fn conditional(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    // 忽略'?'后的新行.
    compiler.ignoreNewLines();

    // 当条件为false时, 跳转到else分支.
    const ifJump = compiler.emitJump(.CODE_JUMP_IF);

    // 编译then分支.
    compiler.parsePrecedence(.PREC_CONDITIONAL);

    compiler.consume(.TOKEN_COLON, "Expect ':' after then branch of conditional operator.");
    compiler.ignoreNewLines();

    // 当if分支被执行时, 跳过else分支.
    const elseJump = compiler.emitJump(.CODE_JUMP);

    // 编译else分支.
    compiler.patchJump(ifJump);

    compiler.parsePrecedence(.PREC_ASSIGNMENT);

    // 填充跳过else的跳转.
    compiler.patchJump(elseJump);
}

fn infixOp(compiler: *Compiler, canAssign: bool) void {
    _ = canAssign;
    const rule = &rules[@intFromEnum(compiler.parser.prev.token_type)];

    // 中缀操作符不能终结表达式.
    compiler.ignoreNewLines();

    // 编译右侧.
    compiler.parsePrecedence(rule.precedence.poffset(1));

    // 对左侧调用操作符方法.
    var signature: Signature = .{ .name = rule.name, .stype = .SIG_METHOD, .arity = 1 };
    compiler.callSignature(.CODE_CALL_0, &signature);
}

// ----------------------------------------------------------------------------------------
fn subscriptSignature(compiler: *Compiler, signature: *Signature) void {
    signature.stype = .SIG_SUBSCRIPT;

    // 当前签名将"["作为其名称, 因为这是匹配它的token. 这里清除掉.
    signature.name.len = 0;

    // 解析下标中的参数.
    compiler.finishParameterList(signature);
    compiler.consume(.TOKEN_RIGHT_BRACKET, "Expect ']' after parameters.");
    _ = compiler.maybeSetter(signature);
}

// 为命名方法或setter编译方法签名.
fn namedSignature(compiler: *Compiler, signature: *Signature) void {
    signature.stype = .SIG_GETTER;
    if (compiler.maybeSetter(signature)) return; // 如果是setter, 它不能有参数列表.
    compiler.parameterList(signature); // 拥有可选参数列表的常规命名方法.
}

// 为构造器编译方法签名.
fn constructorSignature(compiler: *Compiler, signature: *Signature) void {
    compiler.consume(.TOKEN_NAME, "Expect constructor name after 'construct'.");

    // 捕获名称.
    signature.* = compiler.signatureFromToken(.SIG_INITIALIZER);

    if (compiler.match(.TOKEN_EQ)) {
        compiler.doError("A constructor cannot be a setter.", .{});
    }

    if (!compiler.match(.TOKEN_LEFT_PAREN)) {
        compiler.doError("A constructor cannot be a getter.", .{});
        return;
    }

    // 允许空参数列表.
    if (compiler.match(.TOKEN_RIGHT_PAREN)) return;

    compiler.finishParameterList(signature);
    compiler.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

// 为中缀操作符编译方法签名.
fn infixSignature(compiler: *Compiler, signature: *Signature) void {
    // 添加右侧参数.
    signature.stype = .SIG_METHOD;
    signature.arity = 1;

    // 解析参数名称.
    compiler.consume(.TOKEN_LEFT_PAREN, "Expect '(' after operator name.");
    _ = compiler.declareNamedVariable();
    compiler.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
}

// 为一元操作符编译方法签名.
fn unarySignature(compiler: *Compiler, signature: *Signature) void {
    _ = compiler;
    signature.stype = .SIG_GETTER; // 什么也不做, 该名称已经完整.
}

// 为一元或中缀操作符编译方法签名.
fn mixedSignature(compiler: *Compiler, signature: *Signature) void {
    signature.stype = .SIG_GETTER;
    if (compiler.match(.TOKEN_LEFT_PAREN)) {
        // 添加右侧参数.
        signature.stype = .SIG_METHOD;
        signature.arity = 1;

        // 解析参数名称.
        _ = compiler.declareNamedVariable();
        compiler.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
    }
}
