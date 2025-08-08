const std = @import("std");
const V = @import("value.zig");
const VM = @import("vm.zig");
const OpCode = @import("opcode.zig").OpCode;
const Utils = @import("utils.zig");
const C = @import("constants.zig").C;

const G = @import("grammer.zig");
const Signature = G.Signature;
const SignatureType = G.SignatureType;
const Precedence = G.Precedence;

const ZrenVM = VM.ZrenVM;

const Value = V.Value;
const ObjModule = V.ObjModule;
const ObjFunc = V.ObjFunc;
const ObjMap = V.ObjMap;
const ObjString = V.ObjString;

const UsizeBuffer = V.UsizeBuffer;
const ByteBuffer = V.ByteBuffer;
const SymbolTable = V.SymbolTable;

pub const Parser = struct {
    const KEYWORDS: []const Keyword = &.{
        .{ .identifier = "break", .token_type = .TOKEN_BREAK },
        .{ .identifier = "continue", .token_type = .TOKEN_CONTINUE },
        .{ .identifier = "class", .token_type = .TOKEN_CLASS },
        .{ .identifier = "construct", .token_type = .TOKEN_CONSTRUCT },
        .{ .identifier = "else", .token_type = .TOKEN_ELSE },
        .{ .identifier = "false", .token_type = .TOKEN_FALSE },
        .{ .identifier = "for", .token_type = .TOKEN_FOR },
        .{ .identifier = "foreign", .token_type = .TOKEN_FOREIGN },
        .{ .identifier = "if", .token_type = .TOKEN_IF },
        .{ .identifier = "import", .token_type = .TOKEN_IMPORT },
        .{ .identifier = "as", .token_type = .TOKEN_AS },
        .{ .identifier = "in", .token_type = .TOKEN_IN },
        .{ .identifier = "is", .token_type = .TOKEN_IS },
        .{ .identifier = "null", .token_type = .TOKEN_NULL },
        .{ .identifier = "return", .token_type = .TOKEN_RETURN },
        .{ .identifier = "static", .token_type = .TOKEN_STATIC },
        .{ .identifier = "super", .token_type = .TOKEN_SUPER },
        .{ .identifier = "this", .token_type = .TOKEN_THIS },
        .{ .identifier = "true", .token_type = .TOKEN_TRUE },
        .{ .identifier = "var", .token_type = .TOKEN_VAR },
        .{ .identifier = "while", .token_type = .TOKEN_WHILE },
        .{ .identifier = "", .token_type = .TOKEN_EOF },
    };
    vm: *ZrenVM = undefined,
    module: *ObjModule = undefined, // 当前正在解析的模块
    source: []const u8 = &.{}, // 被解析的源码

    token_pos: usize = 0, // 当前被解析的token的起始位置(source中)
    char_pos: usize = 0, // 当前字符位置(source中)
    curr_line: usize = 1, // 当前字符对应的行数(source中, 起始为1)
    next: Token = .{}, // 下一个(即将被处理)
    curr: Token = .{}, // 当前(正在被处理)
    prev: Token = .{}, // 最近一个(消费/前移)
    // 解析插值字符串(如python的f-string)时, 追踪词法分析状态
    //
    // 插值字符串使得词法分析器不是严格正则的:
    //     - 我们无法知道一个`)` 应该被解析为RIGHT_PAREN token还是作为插值表达式的结束
    //     - 除非我们知道我们是否位于插值字符串中, 或有多少未匹配的`(`.
    //
    // 这特别复杂, 因为插值可以嵌套, 如: " %( " %( inner ) " ) "
    //
    // 这里使用这个栈来追踪状态. 解析器维护一个整数栈, 每个整数对应插值字符串的一个层级.
    // 每个整数值代表等待被关闭的未匹配的`(`的数量.
    parens: [C.MAX_INTERPOLATION_NESTING]i32 = .{0} ** C.MAX_INTERPOLATION_NESTING,
    num_parens: usize = 0, //
    print_errors: bool = true, // 打印编译错误或丢弃
    has_error: bool = false, // 是否有语法错误或编译错误

    fn printError(self: *@This(), line: usize, label: []const u8, comptime fmt: []const u8, args: anytype) void {
        self.has_error = true;
        if (!self.print_errors) return;

        const errorFn = self.vm.config.errorFn orelse return;

        var buffer = [_]u8{0} ** C.ERROR_MESSAGE_SIZE;
        const res = std.fmt.bufPrint(buffer[0..], "{s}: ", .{label}) catch unreachable;
        _ = std.fmt.bufPrint(buffer[res.len..], fmt, args) catch unreachable;

        const module_name = if (self.module.name) |name| name.value else "<unknown>";

        errorFn(self.vm, .ERROR_COMPILE, module_name, line, buffer[0..]);
    }

    fn lexError(self: *@This(), comptime fmt: []const u8, args: anytype) void {
        self.printError(self.curr_line, "Error", fmt, args);
    }

    pub fn nextToken(self: *@This()) void {
        self.prev = self.curr;
        self.curr = self.next;

        // 如果已经读取到文件末尾，则返回
        // 仍然复制 TOKEN_EOF 到 prev 以便期望它被消费的代码仍然可以工作
        if (self.next.token_type == .TOKEN_EOF) return;
        if (self.curr.token_type == .TOKEN_EOF) return;

        while (self.char_pos < self.source.len) {
            self.token_pos = self.char_pos;

            const c = self.nextChar();
            switch (c) {
                '(' => {
                    if (self.num_parens > 0) self.parens[self.num_parens - 1] += 1;
                    self.makeToken(.TOKEN_LEFT_PAREN);
                    return;
                },
                ')' => {
                    if (self.num_parens > 0) {
                        self.parens[self.num_parens - 1] -= 1;
                        if (self.parens[self.num_parens - 1] == 0) {
                            self.num_parens -= 1;
                            self.readString();
                            return;
                        }
                    }
                    self.makeToken(.TOKEN_RIGHT_PAREN);
                    return;
                },
                '[' => {
                    self.makeToken(.TOKEN_LEFT_BRACKET);
                    return;
                },
                ']' => {
                    self.makeToken(.TOKEN_RIGHT_BRACKET);
                    return;
                },
                '{' => {
                    self.makeToken(.TOKEN_LEFT_BRACE);
                    return;
                },
                '}' => {
                    self.makeToken(.TOKEN_RIGHT_BRACE);
                    return;
                },
                ':' => {
                    self.makeToken(.TOKEN_COLON);
                    return;
                },
                ',' => {
                    self.makeToken(.TOKEN_COMMA);
                    return;
                },
                '*' => {
                    self.makeToken(.TOKEN_STAR);
                    return;
                },
                '%' => {
                    self.makeToken(.TOKEN_PERCENT);
                    return;
                },
                '#' => {
                    // 忽略第一行中的shebang
                    if (self.curr_line == 1 and self.peekChar() == '!' and self.peekNextChar() == '/') {
                        self.skipLineComment();
                    } else {
                        self.makeToken(.TOKEN_HASH); // 否则当作token处理
                        return;
                    }
                },

                '^' => {
                    self.makeToken(.TOKEN_CARET);
                    return;
                },
                '+' => {
                    self.makeToken(.TOKEN_PLUS);
                    return;
                },
                '-' => {
                    self.makeToken(.TOKEN_MINUS);
                    return;
                },
                '~' => {
                    self.makeToken(.TOKEN_TILDE);
                    return;
                },
                '?' => {
                    self.makeToken(.TOKEN_QUESTION);
                    return;
                },

                '|' => {
                    self.twoCharToken('|', .TOKEN_PIPEPIPE, .TOKEN_PIPE);
                    return;
                },
                '&' => {
                    self.twoCharToken('&', .TOKEN_AMPAMP, .TOKEN_AMP);
                    return;
                },
                '=' => {
                    self.twoCharToken('=', .TOKEN_EQEQ, .TOKEN_EQ);
                    return;
                },
                '!' => {
                    self.twoCharToken('=', .TOKEN_BANGEQ, .TOKEN_BANG);
                    return;
                },
                '.' => {
                    if (self.matchChar('.')) {
                        self.twoCharToken('.', .TOKEN_DOTDOTDOT, .TOKEN_DOTDOT);
                    } else {
                        self.makeToken(.TOKEN_DOT);
                    }
                    return;
                },
                '/' => {
                    if (self.matchChar('/')) {
                        self.skipLineComment();
                    } else if (self.matchChar('*')) {
                        self.skipBlockComment();
                    } else {
                        self.makeToken(.TOKEN_SLASH);
                        return;
                    }
                },
                '<' => {
                    if (self.matchChar('<')) {
                        self.makeToken(.TOKEN_LTLT);
                    } else {
                        self.twoCharToken('=', .TOKEN_LTEQ, .TOKEN_LT);
                    }
                    return;
                },
                '>' => {
                    if (self.matchChar('>')) {
                        self.makeToken(.TOKEN_GTGT);
                    } else {
                        self.twoCharToken('=', .TOKEN_GTEQ, .TOKEN_GT);
                    }
                    return;
                },
                '\n' => {
                    self.makeToken(.TOKEN_LINE);
                    return;
                },
                ' ', '\r', '\t' => {
                    while (self.peekChar() == ' ' or self.peekChar() == '\r' or self.peekChar() == '\t') {
                        _ = self.nextChar();
                    }
                },
                '"' => {
                    if (self.peekChar() == '"' and self.peekNextChar() == '"') {
                        self.readRawString();
                    } else {
                        self.readString();
                    }
                    return;
                },
                '_' => {
                    const t: TokenType = if (self.peekChar() == '_') .TOKEN_STATIC_FIELD else .TOKEN_FIELD;
                    self.readName(t, c);
                    return;
                },
                '0' => {
                    if (self.peekChar() == 'x') {
                        self.readHexNumber();
                    } else {
                        self.readNumber();
                    }
                    return;
                },

                else => {
                    if (Utils.isName(c)) {
                        self.readName(.TOKEN_NAME, c);
                    } else if (Utils.isDigit(c)) {
                        self.readNumber();
                    } else {
                        if (c >= 32 and c <= 126) {
                            self.lexError("Invalid character '{c}'.", .{c});
                        } else {
                            // 不显示非ASCII值，因为我们没有对源代码进行UTF-8解码.
                            // 因为Wren中没有非ASCII字节值是有效的代码单元, 所以解析器只在raw byte上工作,
                            // 即使源代码和终端输出是UTF-8编码的.
                            self.lexError("Invalid byte 0x{x}.", .{c});
                        }
                        self.next.token_type = .TOKEN_ERROR;
                        self.next.char_num = 0;
                    }
                    return;
                },
            }
        }
        // 如果到达源码末尾, 则生成一个EOF token.
        self.token_pos = self.char_pos;
        self.makeToken(.TOKEN_EOF);
    }

    inline fn safePeekChar(self: *@This(), i: usize) u8 {
        return if (i < self.source.len) self.source[i] else '\x00';
    }

    inline fn peekChar(self: *@This()) u8 {
        return if (self.char_pos < self.source.len) self.source[self.char_pos] else '\x00';
    }

    inline fn peekNextChar(self: *@This()) u8 {
        // 如果位于源码末尾, 不则读取下一个字符, 否则读取下一个字符
        return if (self.peekChar() == '\x00') '\x00' else self.safePeekChar(self.char_pos + 1);
    }

    // 向前移动一个字符
    fn nextChar(self: *@This()) u8 {
        const c = self.peekChar();
        self.char_pos += 1;
        if (c == '\n') self.curr_line += 1;
        return c;
    }

    // 如果当前字符是传入的 [c], 则消费它并返回 `true`.
    fn matchChar(self: *@This(), c: u8) bool {
        if (self.peekChar() != c) return false;
        _ = self.nextChar();
        return true;
    }

    // 设置解析器的当前token为给定的 [type] 及字符范围.
    fn makeToken(self: *@This(), token_type: TokenType) void {
        self.next.token_type = token_type;
        self.next.source_ref = self.source;
        self.next.char_start = self.token_pos;
        self.next.char_num = self.char_pos - self.token_pos;
        self.next.line = self.curr_line;

        // 在包含"\n"的行上生成LINE token.
        if (token_type == .TOKEN_LINE) self.next.line -= 1;
    }

    // 如果当前字符是 [c], 则消费它并生成一个类型为 [two] 的token. 否则生成一个类型为 [one] 的token.
    fn twoCharToken(self: *@This(), c: u8, two: TokenType, one: TokenType) void {
        self.makeToken(if (self.matchChar(c)) two else one);
    }

    // 跳过行注释的其余部分.
    fn skipLineComment(self: *@This()) void {
        while (self.peekChar() != '\n' and self.peekChar() != '\x00') _ = self.nextChar();
    }

    // 跳过块注释的其余部分.
    fn skipBlockComment(self: *@This()) void {
        var nesting: i32 = 1;
        while (nesting > 0) {
            if (self.peekChar() == '\x00') {
                self.lexError("Unterminated block comment.", .{});
                return;
            }

            if (self.peekChar() == '/' and self.peekNextChar() == '*') {
                _ = self.nextChar();
                _ = self.nextChar();
                nesting += 1;
                continue;
            }

            if (self.peekChar() == '*' and self.peekNextChar() == '/') {
                _ = self.nextChar();
                _ = self.nextChar();
                nesting -= 1;
                continue;
            }

            // 常规的注释字符.
            _ = self.nextChar();
        }
    }

    // 读取下一个字符, 若是十六进制数字 (0-9, a-f, 或 A-F). 则返回其数值. 否则返回null.
    fn readHexDigit(self: *@This()) ?u8 {
        const c = self.nextChar();
        if (c >= '0' and c <= '9') return c - '0' + 0;
        if (c >= 'a' and c <= 'f') return c - 'a' + 10;
        if (c >= 'A' and c <= 'F') return c - 'A' + 10;
        // 如果不是十六进制数字, 则回退一个字符. (不消费)
        self.char_pos -= 1;
        return null;
    }

    fn makeNumber(self: *@This(), is_hex: bool) void {
        // TODO 改进算法??
        var err: anyerror = undefined;

        const str = self.source[self.token_pos..self.char_pos];
        var num: f64 = 0;

        if (is_hex) {
            const inum = std.fmt.parseInt(i64, str, 0) catch |e| blk: {
                err = e;
                break :blk 0;
            };
            num = @floatFromInt(inum);
        } else {
            num = std.fmt.parseFloat(f64, str) catch |e| blk: {
                err = e;
                break :blk 0;
            };
        }

        if (err == std.fmt.ParseIntError.Overflow) {
            self.lexError("Number literal was too large ({d}).", .{@sizeOf(i64)});
        }
        if (std.math.isInf(num)) {
            self.lexError("Number literal was too large ({d}).", .{@sizeOf(i64)});
        }
        // 在调用 parseInt() 或 parseFloat() 之后, 已经扫描了整个token, 知道它是有效的, 所以不需要检查整个token是否被消耗.
        self.next.value = Value.numToValue(num);
        self.makeToken(.TOKEN_NUMBER);
    }

    // 完成十六进制数字字面量的词法分析.
    fn readHexNumber(self: *@This()) void {
        _ = self.nextChar(); // 跳过上一个用于标识十六进制字面量的的'x'.
        // 读取所有有效的十六进制数字.
        while (self.readHexDigit()) |_| continue;
        self.makeNumber(true);
    }

    // 匹配并读取一个数字
    fn readNumber(self: *@This()) void {
        while (Utils.isDigit(self.peekChar())) _ = self.nextChar();

        // 检查是否有浮点并确保 '.' 后面有数字, 以避免和方法调用混淆.
        if (self.peekChar() == '.' and Utils.isDigit(self.peekNextChar())) {
            _ = self.nextChar();
            while (Utils.isDigit(self.peekChar())) _ = self.nextChar();
        }

        // 检查是否是科学计数法
        if (self.matchChar('e') or self.matchChar('E')) {
            // 允许正负指数符号.
            if (!self.matchChar('+')) {
                _ = self.matchChar('-');
            }

            if (!Utils.isDigit(self.peekChar())) {
                self.lexError("Unterminated scientific notation.", .{});
            }

            while (Utils.isDigit(self.peekChar())) _ = self.nextChar();
        }

        self.makeNumber(false);
    }

    fn readName(self: *@This(), t: TokenType, first_char: u8) void {
        var token_type = t;
        var string = ByteBuffer.init(self.vm.raw_allocator);
        defer string.deinit();
        _ = string.write(first_char);
        while (Utils.isName(self.peekChar()) or Utils.isDigit(self.peekChar())) {
            _ = string.write(self.nextChar());
        }

        const length = self.char_pos - self.token_pos;
        const curr_identifier = self.source[self.token_pos .. self.token_pos + length];
        for (KEYWORDS) |keyword| {
            if (length != keyword.identifier.len) continue;
            if (std.mem.eql(u8, keyword.identifier, curr_identifier)) {
                token_type = keyword.token_type;
                break;
            }
        }

        self.next.value = self.vm.newString(string.data.items[0..string.count]);
        self.makeToken(token_type);
    }

    fn readHexEscape(self: *@This(), digits: usize, description: []const u8) u64 {
        var value: u64 = 0;
        for (0..digits) |_| {
            if (self.peekChar() == '"' or self.peekChar() == '\x00') {
                self.lexError("Incomplete {s} escape sequence.", .{description});
                self.char_pos -= 1; // 不匹配"或\0时, 不消费该字符. 防止读取未终止字符串的末尾部分.
                break;
            }

            const digit = self.readHexDigit() orelse {
                self.lexError("Invalid {s} escape sequence.", .{description});
                break;
            };

            value = (value * 16) | digit;
        }
        return value;
    }

    fn readUnicodeEscape(self: *@This(), string: *ByteBuffer, length: usize) void {
        const value = self.readHexEscape(length, "Unicode");
        const count = Utils.utf8EncodeNumBytes(value);
        if (count != 0) {
            string.fill(0, count); // 申请足够的空间以存储编码后的字符串.
            _ = Utils.utf8Encode(value, string.data.items[string.count - count ..].ptr);
        }
    }

    fn readRawString(self: *@This()) void {
        var string = ByteBuffer.init(self.vm.raw_allocator);
        defer string.deinit();

        // 消费第二个和第三个双引号 "
        _ = self.nextChar();
        _ = self.nextChar();

        var skip_start: ?usize = 0;
        var first_new_line: ?usize = null;

        var skip_end: ?usize = null;
        var last_new_line: ?usize = null;

        while (true) {
            const c0 = self.nextChar();
            const c1 = self.peekChar();
            const c2 = self.peekNextChar();

            if (c0 == '\r') continue;

            if (c0 == '\n') {
                last_new_line = string.count;
                skip_end = string.count;
                if (first_new_line == null) first_new_line = last_new_line; // 设置第一个换行符的位置
            }

            if (c0 == '"' and c1 == '"' and c2 == '"') break;

            const is_white_space = c0 == ' ' or c0 == '\t';
            skip_end = if (c0 == '\n' or is_white_space) skip_end else null;

            // 如果没有遇到换行符或者其它字符, 但遇到空白符, 则认为这些字符可跳过, 直到遇到其它字符为止.
            const skippable = skip_start != null and is_white_space and first_new_line == null;
            skip_start = if (skippable) string.count + 1 else skip_start;

            // 这里已经统计了前导空白字符, 但不是换行符, 所以需要重置skipStart, 因为这些是需要的字符.
            if (first_new_line == null and !is_white_space and c0 != '\n') skip_start = null;

            if (c0 == '\x00' or c1 == '\x00' or c2 == '\x00') {
                self.lexError("Unterminated raw string.", .{});
                // 如果不是预期的字符, 则不消费它. 防止读取未终止字符串的末尾部分.
                self.char_pos -= 1;
                break;
            }
            _ = string.write(c0);
        }

        // 消费第二个和第三个双引号 " ??
        _ = self.nextChar();
        _ = self.nextChar();

        var offset: usize = 0;
        var count: usize = string.count;

        if (first_new_line != null and skip_start == first_new_line) offset = first_new_line.? + 1;
        if (last_new_line != null and skip_end == last_new_line) count = last_new_line.?;

        count -= if (offset > count) count else offset;
        self.next.value = self.vm.newString(string.data.items[offset .. offset + count]);
        self.makeToken(.TOKEN_STRING);
    }

    fn readString(self: *@This()) void {
        var string = ByteBuffer.init(self.vm.raw_allocator);
        defer string.deinit();
        var token_type: TokenType = .TOKEN_STRING;
        while (true) {
            const c = self.nextChar();
            if (c == '"') break;
            if (c == '\r') continue;
            if (c == '\x00') {
                self.lexError("Unterminated string.", .{});
                self.char_pos -= 1;
                break;
            }

            if (c == '%') {
                if (self.num_parens < C.MAX_INTERPOLATION_NESTING) {
                    if (self.nextChar() != '(') self.lexError("Expect '(' after '%'.", .{});
                    self.parens[self.num_parens] = 1;
                    self.num_parens += 1;
                    token_type = .TOKEN_INTERPOLATION;
                    break;
                }
                self.lexError("Interpolation may only nest {d} levels deep.", .{C.MAX_INTERPOLATION_NESTING});
            }
            if (c != '\\') {
                _ = string.write(c);
            } else {
                const next_c = self.nextChar();
                _ = switch (next_c) {
                    '"' => string.write('"'),
                    '\\' => string.write('\\'),
                    '%' => string.write('%'),
                    '0' => string.write('\x00'),
                    'a' => string.write('\x07'),
                    'b' => string.write('\x08'),
                    't' => string.write('\x09'),
                    'e' => string.write('\x1B'),
                    'f' => string.write('\x0C'),
                    'v' => string.write('\x0B'),
                    'n' => string.write('\x0A'),
                    'r' => string.write('\x0D'),
                    'u' => self.readUnicodeEscape(&string, 4),
                    'U' => self.readUnicodeEscape(&string, 8),
                    'x' => string.write(@intCast(self.readHexEscape(2, "byte"))),
                    else => self.lexError("Invalid escape character '{c}'.", .{self.source[self.char_pos - 1]}),
                };
            }
        }

        self.next.value = self.vm.newString(string.data.items[0..string.count]);
        self.makeToken(token_type);
    }
};

pub const Compiler = struct {
    const RULES = G.rules;
    parser: *Parser = undefined,
    parent: ?*@This() = null,
    locals: [C.MAX_LOCALS]Local = [_]Local{.{}} ** C.MAX_LOCALS,
    num_locals: usize = 0,
    upvalues: [C.MAX_UPVALUES]CompilerUpvalue = [_]CompilerUpvalue{.{}} ** C.MAX_UPVALUES,
    scope_depth: i32 = 0,
    num_slots: usize = 0,
    loop: ?*Loop = null,
    enclosing_class: ?*ClassInfo = null,
    func: ?*ObjFunc = null,
    constants: ?*ObjMap = null,
    is_initializer: bool = false,
    num_attrs: usize = 0,
    attributes: ?*ObjMap = null,

    pub fn init(self: *@This(), parser: *Parser, parent: ?*@This(), isMethod: bool) void {
        self.parser = parser;
        self.parent = parent;
        self.loop = null;
        self.enclosing_class = null;
        self.is_initializer = false;

        // 在allocate之前初始化这些为NULL, 以防止GC在初始化编译器过程中被触发.
        self.func = null;
        self.constants = null;
        self.attributes = null;

        parser.vm.compiler = self;

        // 声明一个用于闭包或方法接收者的局部变量槽, 以防止我们尝试重用该槽来存储用户定义的局部变量.
        // 对于方法, 我们将其命名为 "this", 这样就可以像普通变量一样对其解引用.
        // 对于函数, 它们没有显式的 "this", 因此使用一个空名称.
        // 这样对 "this" 的引用就可以沿着父链向上查找, 找到包含该函数的方法, 该方法可以关闭 "this".
        self.num_locals = 1;
        self.num_slots = self.num_locals;

        self.locals[0].name = if (isMethod) "this" else &.{};

        self.locals[0].depth = -1;
        self.locals[0].is_upvalue = false;

        self.scope_depth = if (parent != null) 0 else -1;

        self.num_attrs = 0;
        self.attributes = parser.vm.newMap();
        self.func = parser.vm.newFunction(parser.module, self.num_locals);
    }

    pub fn parsePrecedence(self: *@This(), precedence: Precedence) void {
        self.parser.nextToken();
        const prefix = RULES[self.parser.prev.token_type.num()].prefix orelse {
            self.doError("Expected expression.", .{});
            return;
        };

        // 跟踪周围表达式的优先级是否足够低, 以允许在此处进行赋值.
        // 我们不能像普通表达式那样编译赋值, 因为它需要我们特别处理左侧, 它需要是一个lvalue, 而不是一个rvalue.
        // 因此, 对于每一个是合法lvalue名, 下标, 字段等类型的表达式, 我们都会传入是否允许"="的上下文松散程度.
        // 如果是, 它将解析 "=" 本身并适当地处理它.
        const can_assign = precedence.num() <= Precedence.PREC_CONDITIONAL.num();
        prefix(self, can_assign);

        while (precedence.num() <= RULES[self.parser.curr.token_type.num()].precedence.num()) {
            self.parser.nextToken();
            const infix = RULES[self.parser.prev.token_type.num()].infix orelse continue;
            infix(self, can_assign);
        }
    }

    pub fn expression(self: *@This()) void {
        self.parsePrecedence(.PREC_LOWEST);
    }

    // 标记loop的起始. 保持追踪当前指令, 以便在body结束时知道要回到哪里.
    fn startLoop(self: *@This(), loop: *Loop) void {
        loop.enclosing = self.loop;
        loop.start = self.func.?.code.count;
        loop.scope_depth = self.scope_depth;
        self.loop = loop;
    }

    // 生成用于测试循环条件的[CODE_JUMP_IF]指令, 并可能退出循环.
    // 保持追踪该指令, 已便后续知道body的结束位置时, 可以进行jumpPatch填充.
    fn testExitLoop(self: *@This()) void {
        self.loop.?.exit_jump = self.emitJump(.CODE_JUMP_IF);
    }

    // 编译循环体并跟踪其范围, 以便可以正确处理包含的 "break" 语句.
    fn loopBody(self: *@This()) void {
        self.loop.?.body = self.func.?.code.count;
        self.statement();
    }

    // 结束最内存的循环. 现在知道循环的结束位置, 所以可以patch所有的jump和break.
    fn endLoop(self: *@This()) void {
        // 不检查溢出, 因为循环体前向跳转会报告相同的错误.

        const func = self.func orelse return;
        const loop = self.loop orelse return;
        // TODO test/limit/loop_too_far.wren 会导致这里溢出! 需要改进(临时用truncate)
        const loop_offset: u16 = @truncate(func.code.count - loop.start + 3);
        self.emitShortArg(.CODE_LOOP, loop_offset);

        self.patchJump(loop.exit_jump);

        // 查找任何break占位符指令(在字节码中将是CODE_END), 并用真正的jump替换.
        var i = loop.body;
        while (i < func.code.count) {
            if (func.code.at(i) == OpCode.CODE_END.num()) {
                func.code.rat(i).* = OpCode.CODE_JUMP.num();
                self.patchJump(i + 1);
                i += 3;
            } else {
                i += 1 + func.getByteCountForArguments(i); // 跳过这个指令及其参数.
            }
        }

        self.loop = loop.enclosing;
    }

    // 为具有[signature]和[initializer_symbol]的初始化器创建匹配的构造函数方法
    //
    // 在Wren中, 构造是一个两阶段的过程, 它涉及两个单独的方法:
    //   1. 有一个静态方法分配该类的新实例.
    //   2. 然后, 它在新实例上调用一个初始化方法, 将所有构造函数参数转发给它.
    // 分配器方法有固定实现:
    //     CODE_CONSTRUCT - 将槽位0中的类替换为它的一个新实例.
    //     CODE_CALL      - 在新实例上调用初始化方法.
    //
    // 该函数创建该方法并根据[initializer_symbol]调用初始化器.
    fn createConstructor(self: *@This(), signature: *Signature, initializer_symbol: u16) void {
        var c: Compiler = .{};
        c.init(self.parser, self, true);

        const op: OpCode = if (self.enclosing_class.?.is_foreign) .CODE_FOREIGN_CONSTRUCT else .CODE_CONSTRUCT;
        c.emitOp(op); // 分配新的实例
        c.emitShortArg(OpCode.CODE_CALL_0.poffset(signature.arity), initializer_symbol); // 运行初始化方法
        c.emitOp(.CODE_RETURN); // 返回实例
        _ = c.endCompiler("");
    }

    // 加载封闭类到栈上, 然后将栈上的函数绑定到该类的方法上.
    fn defineMethod(self: *@This(), classVariable: Variable, isStatic: bool, symbol: u16) void {
        // 加载class. 需要对每个方法都这样做, 因为不能将class保持在栈顶.
        // 如果存在静态字段, 它们将在栈上初始变量槽位之上的局部变量中.
        // 为了跳过这些, 我们每次在定义方法之前都加载class.
        self.loadVariable(classVariable);

        // 定义方法
        const instruction: OpCode = if (isStatic) .CODE_METHOD_STATIC else .CODE_METHOD_INSTANCE;
        self.emitShortArg(instruction, symbol);
    }

    // 根据[signature]声明封闭类中的方法. 若已声明该签名的函数, 则报告错误.
    fn declareMethod(self: *@This(), signature: *Signature, name: []const u8) u16 {
        const symbol = self.signatureSymbol(signature);

        // 检查类是否已经声明了具有此签名的函数.
        const class_info = self.enclosing_class orelse unreachable;
        const methods = if (class_info.in_static) &class_info.static_methods else &class_info.methods;
        for (0..methods.count) |i| {
            if (methods.at(i) != symbol) continue;
            const staticPrefix = if (class_info.in_static) "static " else "";
            self.doError("Class {s} already defines a {s}method '{s}'.", .{ class_info.name.value, staticPrefix, name });
            break;
        }

        methods.push(symbol);
        return symbol;
    }

    fn consumeLiteral(self: *@This(), comptime message: []const u8) Value {
        if (self.match(.TOKEN_FALSE)) return .FALSE_VAL;
        if (self.match(.TOKEN_TRUE)) return .TRUE_VAL;
        if (self.match(.TOKEN_NUMBER)) return self.parser.prev.value;
        if (self.match(.TOKEN_STRING)) return self.parser.prev.value;
        if (self.match(.TOKEN_NAME)) return self.parser.prev.value;

        self.doError(message, .{});
        self.parser.nextToken();
        return .NULL_VAL;
    }

    fn matchAttribute(self: *@This()) bool {
        if (!self.match(.TOKEN_HASH)) return false;

        self.num_attrs += 1;
        const runtime_access = self.match(.TOKEN_BANG);
        if (!self.match(.TOKEN_NAME)) self.doError("Expect an attribute definition after #.", .{});

        const group = self.parser.prev.value;
        const ahead = self.peek();
        if (ahead == .TOKEN_EQ or ahead == .TOKEN_LINE) {
            const key = group;
            var value: Value = .NULL_VAL;
            if (self.match(.TOKEN_EQ)) {
                value = self.consumeLiteral("Expect a Bool, Num, String or Identifier literal for an attribute value.");
            }
            if (runtime_access) self.addToAttributeGroup(.NULL_VAL, key, value);
        } else if (self.match(.TOKEN_LEFT_PAREN)) {
            self.ignoreNewLines();
            if (self.match(.TOKEN_RIGHT_PAREN)) {
                self.doError("Expected attributes in group, group cannot be empty.", .{});
            } else {
                while (self.peek() != .TOKEN_RIGHT_PAREN) {
                    self.consume(.TOKEN_NAME, "Expect name for attribute key.");
                    const key = self.parser.prev.value;
                    var value: Value = .NULL_VAL;
                    if (self.match(.TOKEN_EQ)) {
                        value = self.consumeLiteral("Expect a Bool, Num, String or Identifier literal for an attribute value.");
                    }
                    if (runtime_access) self.addToAttributeGroup(group, key, value);
                    self.ignoreNewLines();
                    if (!self.match(.TOKEN_COMMA)) break;
                    self.ignoreNewLines();
                }

                self.ignoreNewLines();
                self.consume(.TOKEN_RIGHT_PAREN, "Expected ')' after grouped attributes.");
            }
        } else {
            self.doError("Expect an equal, newline or grouping after an attribute key.", .{});
        }

        self.consumeLine("Expect newline after attribute.");
        return true;
    }

    // 编译class 代码块中的成员函数. 如果编译成功，返回true，否则返回false.
    fn method(self: *@This(), class_var: Variable) bool {
        // 在解析方法之前解, 先析属性并将其存储
        if (self.matchAttribute()) return self.method(class_var);

        const is_foreign = self.match(.TOKEN_FOREIGN);
        const is_static = self.match(.TOKEN_STATIC);
        self.enclosing_class.?.in_static = is_static;

        const _signatureFn = RULES[self.parser.curr.token_type.num()].method;
        self.parser.nextToken();
        const signatureFn = _signatureFn orelse {
            self.doError("Expect method definition.", .{});
            return false;
        };

        // 构建方法签名.
        var signature = self.signatureFromToken(.SIG_GETTER);
        self.enclosing_class.?.signature = &signature;

        var method_compiler: Compiler = .{};
        method_compiler.init(self.parser, self, true);

        // 编译方法签名.
        signatureFn(&method_compiler, &signature);

        method_compiler.is_initializer = signature.stype == .SIG_INITIALIZER;

        if (is_static and signature.stype == .SIG_INITIALIZER) {
            self.doError("A constructor cannot be static.", .{});
        }
        // 在debug消息中包含完整的签名，以便在堆栈跟踪中显示.
        var full_sign = [_]u8{0} ** C.MAX_METHOD_SIGNATURE;
        const fs = signature.toString(full_sign[0..]);
        // 拷贝编译器收集的任何属性到封闭类中.
        self.copyMethodAttributes(is_foreign, is_static, fs);

        // 检查方法是否已经定义.
        const method_symbol = self.declareMethod(&signature, fs); // 这里只是为 method 分配一个符号，并没有真正定义

        if (is_foreign) {
            // 对该签名进行常量定义
            self.emitConstant(self.parser.vm.newString(fs));
            method_compiler.parser.vm.compiler = method_compiler.parent.?;
        } else {
            self.consume(.TOKEN_LEFT_BRACE, "Expect '{{' to begin method body.");
            method_compiler.finishBody();
            _ = method_compiler.endCompiler(fs);
        }

        // 定义方法. 对于构造函数，这里是定义实例初始化方法.
        self.defineMethod(class_var, is_static, method_symbol);

        if (signature.stype == .SIG_INITIALIZER) {
            // 同时在元类上定义一个匹配的构造函数方法.
            signature.stype = .SIG_METHOD;
            const ctor_symbol = self.signatureSymbol(&signature);

            self.createConstructor(&signature, method_symbol);
            self.defineMethod(class_var, true, ctor_symbol);
        }

        return true;
    }

    // 编译类定义. 假定该"class"的token已经被消费了（以及可能的前置"foreign" token）.
    fn classDefinition(self: *@This(), is_foreign: bool) void {
        // 创建一个变量已存储类.
        const class_var: Variable = .{ .scope = if (self.scope_depth == -1) .SCOPE_MODULE else .SCOPE_LOCAL, .index = self.declareNamedVariable() };

        // 创建共享的类名值.
        const class_name_str: Value = self.parser.vm.newString(self.parser.prev.name());

        // 创建类名字符串以跟踪方法重复项
        const class_name = class_name_str.asString();

        // 为类名创建一个字符串常量.
        self.emitConstant(class_name_str);

        if (self.match(.TOKEN_IS)) {
            self.parsePrecedence(.PREC_CALL); // 如果存在，加载超类
        } else {
            self.loadCoreVariable("Object"); //  隐式继承Object
        }

        // 为类字段数量预留一个占位符. 在编译所有方法以查看哪些字段被使用前，并不知道具体数量.
        var num_fields_instruction: usize = 0;
        if (is_foreign) {
            self.emitOp(.CODE_FOREIGN_CLASS);
        } else {
            num_fields_instruction = self.emitByteArg(.CODE_CLASS, 255);
        }

        // 将类存储在其名称中.
        self.defineVariable(@intCast(class_var.index));

        // 将局部变量push到作用域. 类中的静态字段会被提升到这个作用域中，方法会通过upvalues引用它们.
        self.pushScope();

        var class_info: ClassInfo = .{
            .is_foreign = is_foreign,
            .name = class_name,
            // 如果需要，分配属性map.
            .class_attrs = if (self.attributes.?.entries.count() > 0) self.parser.vm.newMap() else null,
            .method_attrs = null,
        };

        self.copyAttributes(class_info.class_attrs); // 拷贝已解析的属性到类中.

        // 为类字段设置符号表. 初始时，字段从零开始编号.
        // 当方法被绑定到类时，字节码将通过[bindMethod]进行调整, 以考虑继承的字段.
        class_info.fields = SymbolTable.init(self.parser.vm.allocator);

        // 设置符号缓冲区以跟踪重复的静态和实例方法.
        class_info.methods = UsizeBuffer.init(self.parser.vm.allocator);
        class_info.static_methods = UsizeBuffer.init(self.parser.vm.allocator);
        self.enclosing_class = &class_info;

        // 编译方法定义.
        self.consume(.TOKEN_LEFT_BRACE, "Expect '{{' after class declaration.");
        _ = self.matchLine();

        while (!self.match(.TOKEN_RIGHT_BRACE)) {
            if (!self.method(class_var)) break;

            // 在最后一个定义后不需要换行符.
            if (self.match(.TOKEN_RIGHT_BRACE)) break;

            self.consumeLine("Expect newline after definition in class.");
        }

        // 如果存在属性, 为class创建一个ClassAttributes实例，并将其传递给CODE_END_CLASS
        const has_attr = class_info.class_attrs != null or class_info.method_attrs != null;
        if (has_attr) {
            self.emitClassAttributes(&class_info);
            self.loadVariable(class_var);
            // 目前，不需要其他用途的CODE_END_CLASS，所以可以将其放在这个condition中.
            // 稍后，可以随时生成它并按需使用.
            self.emitOp(.CODE_END_CLASS);
        }

        // 根据字段数更新类.
        if (!is_foreign) {
            self.func.?.code.rat(num_fields_instruction).* = @truncate(class_info.fields.count);
        }

        // 清除符号表以跟踪字段和方法名称.
        class_info.fields.clear();
        class_info.methods.clear();
        class_info.static_methods.clear();
        self.enclosing_class = null;
        self.popScope();
    }

    // 编译import语句. 一个import会被编译为一系列指令. 给定:
    //     import "foo" for Bar, Baz
    // 首先，我们编译一个IMPORT_MODULE "foo"指令来加载模块本身.
    // 当它完成执行时，导入的模块会将其ObjModule留在vm->last_module中.
    // 然后，对于Bar和Baz，我们:
    //   1. 在当前作用域中声明一个变量，使用该名称.
    //   2. 编译一个IMPORT_VARIABLE指令，从刚才的模块加载变量的值.
    //   3. 然后编译一个STORE_LOCAL指令，将变量的值存储在当前作用域中.
    fn import(self: *@This()) void {
        self.ignoreNewLines();
        self.consume(.TOKEN_STRING, "Expect a string after 'import'.");
        const module_constant = self.addConstant(self.parser.prev.value) orelse std.math.maxInt(usize); // TODO 这里不能panic, 而应继续执行??
        // 加载模块
        self.emitShortArg(.CODE_IMPORT_MODULE, @truncate(module_constant));

        // 从调用模块体的闭包中丢弃未使用的结果值.
        self.emitOp(.CODE_POP);

        // 对于导入语句, "for" 是可选的.
        if (!self.match(.TOKEN_FOR)) return;

        // 编译逗号分隔的变量导入列表.
        while (true) {
            self.ignoreNewLines();

            self.consume(.TOKEN_NAME, "Expect variable name.");

            // 我们需要保留源变量，以便后续引用它.
            var source_var_token = self.parser.prev;

            // 为原始变量名定义一个字符串常量.
            const source_var_constant = self.addConstant(self.parser.vm.newString(source_var_token.name())) orelse @panic("import: Add Constant failed");

            // 存储所关注的变量符号
            var slot: u16 = 0;
            if (self.match(.TOKEN_AS)) {
                // import "module" for Source as Dest
                // 使用 'Dest' 作为名称来声明新变量.
                // 这会解析 'as' 后面的名称并定义它.
                slot = self.declareNamedVariable();
            } else {
                // import "module" for Source
                // 直接使用 'Source' 作为名称来声明新变量.
                slot = self.declareVariable(&source_var_token);
            }

            // 从模块中加载所需变量.
            self.emitShortArg(.CODE_IMPORT_VARIABLE, @intCast(source_var_constant));

            // 存储结果.
            self.defineVariable(slot);
            if (!self.match(.TOKEN_COMMA)) break;
        }
    }

    // 处理 "var" 变量定义语句.
    fn variableDefinition(self: *@This()) void {
        // 获取其名称，但不要立即声明它. 局部变量不应位于其自身的初始化器的作用域内.
        self.consume(.TOKEN_NAME, "Expect variable name.");
        var name_token = self.parser.prev;

        // 编译初始化器.
        if (self.match(.TOKEN_EQ)) {
            self.ignoreNewLines();
            self.expression();
        } else {
            // 默认初始化为null.
            self.null_(false);
        }

        // 现在将其放入作用域中.
        const symbol = self.declareVariable(&name_token);
        self.defineVariable(symbol);
    }

    pub fn definition(self: *@This()) void {
        if (self.matchAttribute()) {
            self.definition();
            return;
        }

        if (self.match(.TOKEN_CLASS)) {
            self.classDefinition(false);
            return;
        }

        if (self.match(.TOKEN_FOREIGN)) {
            self.consume(.TOKEN_CLASS, "Expect 'class' after 'foreign'.");
            self.classDefinition(true);
            return;
        }

        self.disallowAttributes();
        if (self.match(.TOKEN_IMPORT)) {
            self.import();
        } else if (self.match(.TOKEN_VAR)) {
            self.variableDefinition();
        } else {
            self.statement();
        }
    }

    // 返回当前token的类型.
    pub inline fn peek(self: *@This()) TokenType {
        return self.parser.curr.token_type;
    }

    // 返回下一个token的类型.
    inline fn peekNext(self: *@This()) TokenType {
        return self.parser.next.token_type;
    }

    // 根据[expected]消费当前token. 如果匹配到则返回true. 否则返回false.
    pub fn match(self: *@This(), expected: TokenType) bool {
        if (self.peek() != expected) return false;
        self.parser.nextToken();
        return true;
    }

    // 消费当前token. 如果token不匹配[expected], 则抛出错误.
    pub fn consume(self: *@This(), expected: TokenType, comptime errorMsg: []const u8) void {
        self.parser.nextToken();
        if (self.parser.prev.token_type != expected) {
            self.doError(errorMsg, .{});
            if (self.parser.curr.token_type == expected) self.parser.nextToken();
        }
    }

    // 匹配任意个换行符, 如果匹配到了则返回true.
    pub fn matchLine(self: *@This()) bool {
        if (!self.match(.TOKEN_LINE)) return false;
        while (self.match(.TOKEN_LINE)) {}
        return true;
    }

    // 丢弃所有紧随其后的换行符.
    pub fn ignoreNewLines(self: *@This()) void {
        _ = self.matchLine();
    }

    // 消费当前token. 如果其不是换行, 则抛出错误. 后续丢弃任何紧随其后的重复换行符.
    fn consumeLine(self: *@This(), comptime errorMsg: []const u8) void {
        self.consume(.TOKEN_LINE, errorMsg);
        self.ignoreNewLines();
    }

    pub fn allowLineBeforeDot(self: *@This()) void {
        if (self.peek() == .TOKEN_LINE and self.peekNext() == .TOKEN_DOT) {
            self.parser.nextToken();
        }
    }

    fn disallowAttributes(self: *@This()) void {
        if (self.num_attrs <= 0) return;

        self.doError("Attributes can only specified before a class or a method", .{});
        self.attributes.?.clear();
        self.num_attrs = 0;
    }

    // 添加一个属性到编译器属性map中的给定组中.
    fn addToAttributeGroup(self: *@This(), group: Value, key: Value, value: Value) void {
        const vm = self.parser.vm;

        if (group.isObj()) vm.pushRoot(group.asObj());
        if (key.isObj()) vm.pushRoot(key.asObj());
        if (value.isObj()) vm.pushRoot(value.asObj());

        var groupMapValue = self.attributes.?.mapGet(group);
        if (groupMapValue.isUndefined()) {
            groupMapValue = vm.newMap().asObj().toVal();
            self.attributes.?.mapSet(group, groupMapValue);
        }

        // 将它们存储为每个组的map，以便我们可以维护重复的键 group = { key:[value, ...], }
        const groupMap = groupMapValue.asMap();

        // var keyItems = group[key]
        // if(!keyItems) keyItems = group[key] = []
        var keyItemsValue = groupMap.mapGet(key);
        if (keyItemsValue.isUndefined()) {
            keyItemsValue = vm.newList(0).asObj().toVal();
            groupMap.mapSet(key, keyItemsValue);
        }

        // keyItems.add(value)
        const keyItems = keyItemsValue.asList();
        keyItems.elements.push(value);

        if (group.isObj()) vm.popRoot();
        if (key.isObj()) vm.popRoot();
        if (value.isObj()) vm.popRoot();
    }

    // 变量和作用域 -----------------------------------------------------------------

    // 生成单个单字节参数, 并返回其索引.
    pub fn emitByte(self: *@This(), byte: u8) usize {
        self.func.?.code.push(byte);
        // 假定该指令与最近被消费的token相关联.
        self.func.?.debug.source_lines.push(self.parser.prev.line);
        return self.func.?.code.count - 1;
    }

    // 生成单个两字节参数, 按大端序写入.
    pub fn emitShort(self: *@This(), arg: u16) void {
        _ = self.emitByte(@intCast((arg >> 8) & 0xff));
        _ = self.emitByte(@intCast((arg >> 0) & 0xff));
    }

    // 生成单个字节码指令.
    pub fn emitOp(self: *@This(), op: OpCode) void {
        _ = self.emitByte(op.num());
        // 保持追踪栈的峰值.
        self.num_slots +%= @bitCast(C.StackEffects[op.num()].effect);
        if (self.num_slots > self.func.?.max_slots) self.func.?.max_slots = self.num_slots;
    }

    // 生成一个伴有单字节参数的字节码指令. 返回参数的索引.
    pub fn emitByteArg(self: *@This(), op: OpCode, arg: u8) usize {
        self.emitOp(op);
        return self.emitByte(arg);
    }

    // 生成一个伴有双字节参数的字节码指令. 按大端序写入.
    pub fn emitShortArg(self: *@This(), op: OpCode, arg: u16) void {
        self.emitOp(op);
        self.emitShort(arg);
    }

    // 生成一个伴有jump offset偏移占位符的字节码指令. 返回占位符的索引.
    // 该占位符可以通过调用[jumpPatch]来填充.
    pub fn emitJump(self: *@This(), op: OpCode) usize {
        self.emitOp(op);
        _ = self.emitByte(0xff);
        return self.emitByte(0xff) - 1;
    }

    // 对传入的value创建一个新常量, 并生用于从常量表加载该常量的字节码.
    pub fn emitConstant(self: *@This(), value: Value) void {
        const constant = self.addConstant(value) orelse 0;
        self.emitShortArg(.CODE_CONSTANT, @intCast(constant)); // 生成加载该常量的code
    }

    // 在栈上生成给定map中的属性.
    fn emitAttributes(self: *@This(), attributes: *ObjMap) void {
        // 为属性初始化一个新map
        self.loadCoreVariable("Map");
        self.callMethod(0, "new()");

        // 属性被存储为 group = { key:[value, value, ...] } , 因此第一层是group map
        var iter = attributes.entries.iterator();
        while (iter.next()) |groupEntry| {

            // 属性group的 key
            self.emitConstant(groupEntry.key_ptr.*);

            // 属性group的value 也是一个map
            self.loadCoreVariable("Map");
            self.callMethod(0, "new()");

            const groupItems = groupEntry.value_ptr.asMap();
            var itemIter = groupItems.entries.iterator();
            while (itemIter.next()) |itemEntry| {
                self.emitConstant(itemEntry.key_ptr.*);
                // 属性的 key value: key = []
                self.loadCoreVariable("List");
                self.callMethod(0, "new()");
                const items = itemEntry.value_ptr.asList(); // 添加项到key列表

                for (0..items.elements.count) |itemIdx| {
                    self.emitConstant(items.elements.at(itemIdx));
                    self.callMethod(1, "addCore_(_)");
                }
                self.callMethod(2, "addCore_(_,_)"); // 添加list到map
            }

            self.callMethod(2, "addCore_(_,_)"); // 添加key/value到 map
        }
    }

    // 方法被存储为 method <-> attributes, 所以我们需要一个间接层来解析方法
    fn emitAttributeMethods(self: *@This(), attributes: *ObjMap) void {
        // 为属性初始化一个新map
        self.loadCoreVariable("Map");
        self.callMethod(0, "new()");

        var iter = attributes.entries.iterator();
        while (iter.next()) |methodEntry| {
            self.emitConstant(methodEntry.key_ptr.*);
            const attributeMap = methodEntry.value_ptr.asMap();
            self.emitAttributes(attributeMap);
            self.callMethod(2, "addCore_(_,_)");
        }
    }

    // 生成最终的ClassAttributes, 包含所有属性, 该属性会存在于运行时
    fn emitClassAttributes(self: *@This(), classInfo: *ClassInfo) void {
        self.loadCoreVariable("ClassAttributes");

        if (classInfo.class_attrs) |ca| self.emitAttributes(ca) else self.null_(false);
        if (classInfo.method_attrs) |ma| self.emitAttributeMethods(ma) else self.null_(false);

        self.callMethod(2, "new(_,_)");
    }

    // 拷贝当前存储在编译器中的属性到目标map中, 同时清除源map, 因为意图是消费这些属性
    fn copyAttributes(self: *@This(), into: ?*ObjMap) void {
        self.num_attrs = 0;
        const dest = into orelse return;
        const src = self.attributes orelse return;
        if (src.entries.count() == 0) return;
        dest.clear();
        dest.entries = src.entries.move();
    }

    // 拷贝当前存储在编译器中的属性到当前enclosing_class的method特定属性中.
    // 这也会重置计数器, 因为意图是消费这些属性
    fn copyMethodAttributes(self: *@This(), is_foreign: bool, is_static: bool, full_sign: []const u8) void {
        self.num_attrs = 0;

        var attributes = self.attributes orelse return;
        if (attributes.entries.count() == 0) return;

        const vm = self.parser.vm;
        // 为当前method创建一个map, 用于拷贝属性
        const method_attr = vm.newMap();
        vm.pushRoot(method_attr.asObj());
        self.copyAttributes(method_attr);

        // 如果必要的话, 在前面加上 'foreign static '
        var full_len = full_sign.len;
        if (is_foreign) full_len += 8;
        if (is_static) full_len += 7;
        var full_sign_with_prefix = [_]u8{0} ** (C.MAX_METHOD_SIGNATURE + 8 + 7);
        const fPrefix = if (is_foreign) "foreign " else "";
        const sPrefix = if (is_static) "static " else "";
        _ = std.fmt.bufPrint(full_sign_with_prefix[0..], "{s}{s}{s}", .{ fPrefix, sPrefix, full_sign }) catch unreachable;

        if (self.enclosing_class.?.method_attrs == null) {
            self.enclosing_class.?.method_attrs = vm.newMap();
        }

        // 存储方法属性到类map中
        const key = vm.newString(full_sign_with_prefix[0..full_len]);
        self.enclosing_class.?.method_attrs.?.mapSet(key, method_attr.asObj().toVal());

        vm.popRoot();
    }

    pub fn addConstant(self: *@This(), constant: Value) ?usize {
        if (self.parser.has_error) return null;

        // 检查是否已经存在该常量. 如果存在, 则重用
        if (self.constants) |constants| {
            const existing = constants.mapGet(constant);
            if (existing.isNum()) return @intFromFloat(existing.asNum());
        }

        // 添加新常量
        var func = self.func orelse @panic("Func cant be null!");
        if (func.constants.count < C.MAX_CONSTANTS) {
            if (constant.isObj()) self.parser.vm.pushRoot(constant.asObj());
            func.constants.push(constant);
            if (constant.isObj()) self.parser.vm.popRoot();

            if (self.constants == null) self.constants = self.parser.vm.newMap();

            self.constants.?.mapSet(constant, Value.numToValue(@floatFromInt(func.constants.count - 1)));
        } else {
            self.doError("A function may only contain {d} unique constants.", .{C.MAX_CONSTANTS});
        }

        return func.constants.count - 1;
    }

    pub fn endCompiler(self: *@This(), debug_name: []const u8) ?*ObjFunc {
        // 如果遇到错误, 不需要完成函数, 因为其已经被破坏.
        if (self.parser.has_error) {
            self.parser.vm.compiler = self.parent;
            return null;
        }
        // 标记字节码的终止. 由于它可能包含多个早期return, 所以不能依赖CODE_RETURN来表示到达末尾.
        self.emitOp(.CODE_END);
        self.parser.vm.functionBindName(self.func, debug_name);
        if (self.parent) |p| {
            const constant = p.addConstant(self.func.?.asObj().toVal());
            p.emitShortArg(.CODE_CLOSURE, @intCast(constant.?));

            // 为每个upvalue生成字节码, 以便知道是否捕获局部变量或upvalue.
            for (0..self.func.?.num_upvalues) |i| {
                _ = p.emitByte(if (self.upvalues[i].is_local) 1 else 0);
                _ = p.emitByte(@intCast(self.upvalues[i].index));
            }
        }

        self.parser.vm.compiler = self.parent;
        // TODO 添加debug信息
        // self.parser.vm.dumpCode(self.func);

        return self.func;
    }

    pub fn doError(self: *@This(), comptime fmt: []const u8, args: anytype) void {
        const token = &self.parser.prev;
        switch (token.token_type) {
            .TOKEN_ERROR => return,
            .TOKEN_LINE => self.parser.printError(token.line, "Error at newline", fmt, args),
            .TOKEN_EOF => self.parser.printError(token.line, "Error at end of file", fmt, args),
            else => {
                var buf = [_]u8{0} ** (10 + C.MAX_VARIABLE_NAME + 4 + 1);
                var label: []const u8 = undefined;
                if (token.char_num <= C.MAX_VARIABLE_NAME) {
                    label = std.fmt.bufPrint(buf[0..], "Error at '{s}'", .{token.name()}) catch unreachable;
                } else {
                    label = std.fmt.bufPrint(buf[0..], "Error at '{s}...'", .{token.name()[0..C.MAX_VARIABLE_NAME]}) catch unreachable;
                }
                self.parser.printError(token.line, label, fmt, args);
            },
        }
    }

    pub fn loadLocal(self: *@This(), slot: u8) void {
        if (slot <= 8) {
            self.emitOp(OpCode.load_local_n(slot));
        } else {
            _ = self.emitByteArg(.CODE_LOAD_LOCAL, slot);
        }
    }

    pub fn loadVariable(self: *@This(), variable: ?Variable) void {
        if (variable) |v| switch (v.scope) {
            .SCOPE_LOCAL => self.loadLocal(@truncate(v.index)),
            .SCOPE_UPVALUE => _ = self.emitByteArg(.CODE_LOAD_UPVALUE, @truncate(v.index)),
            .SCOPE_MODULE => self.emitShortArg(.CODE_LOAD_MODULE_VAR, @truncate(v.index)),
        };
    }

    // 加载当前封闭方法的方法接收者. 正确处理定义在方法内的函数.
    pub inline fn loadThis(self: *@This()) void {
        self.loadVariable(self.resolveNonmodule("this"));
    }

    // 推入一个value用于从core中隐式导入的模块级变量.
    pub fn loadCoreVariable(self: *@This(), name: []const u8) void {
        const symbol = self.parser.vm.symbolTableFind(&self.parser.module.variable_names, name) orelse @panic("Should have already defined core name.");
        self.emitShortArg(.CODE_LOAD_MODULE_VAR, @intCast(symbol));
    }

    // 根据[name]创建一个新的局部变量. 假设当前是局部作用域, 并且名称是唯一的.
    pub fn addLocal(self: *@This(), name: []const u8) usize {
        var local = &self.locals[self.num_locals];
        local.name = name;
        local.depth = self.scope_depth;
        local.is_upvalue = false;
        self.num_locals += 1;
        return self.num_locals - 1;
    }

    // 在当前作用域中存储一个具有先前定义的符号的变量.
    pub fn declareVariable(self: *@This(), itoken: ?*Token) u16 {
        const token = itoken orelse &self.parser.prev;
        if (token.char_num > C.MAX_VARIABLE_NAME) {
            self.doError("Variable name cannot be longer than {d} characters.", .{C.MAX_VARIABLE_NAME});
        }

        // 顶层模块作用域.
        if (self.scope_depth == -1) {
            var line: i32 = -1;
            const symbol = self.parser.vm.defineVariable(self.parser.module, token.name(), &.NULL_VAL, &line);

            switch (symbol) {
                -1 => self.doError("Module variable is already defined.", .{}),
                -2 => self.doError("Too many module variables defined.", .{}),
                -3 => self.doError("Variable '{s}' referenced before this definition (first use at line {d}).", .{ token.name(), line }),
                else => {},
            }

            return @bitCast(@as(i16, @intCast(symbol))); // TODO symbol可能为负, 所以用bitCast
        }

        // 检查当前作用域中是否已经声明了具有此名称的变量(外部作用域是允许的: 会被遮蔽)
        var i = self.num_locals;
        while (i > 0) {
            i -= 1;
            const local = &self.locals[i];
            // 一旦跳出当前作用域并进入外部作用域时, 停止.
            if (local.depth < self.scope_depth) break;

            if (std.mem.eql(u8, local.name, token.name())) {
                self.doError("Variable is already declared in this scope.", .{});
                return @intCast(i);
            }
        }

        if (self.num_locals == C.MAX_LOCALS) {
            self.doError("Cannot declare more than {d} variables in one scope.", .{C.MAX_LOCALS});
            return @bitCast(@as(i16, @intCast(-1)));
        }

        return @intCast(self.addLocal(token.name()));
    }

    // 解析一个name token并在当前作用域中声明该名称的变量. 返回其槽位.
    pub fn declareNamedVariable(self: *@This()) u16 {
        self.consume(.TOKEN_NAME, "Expect variable name.");
        return self.declareVariable(null);
    }

    // 在当前作用域中存储一个具有先前定义的符号的变量.
    pub fn defineVariable(self: *@This(), symbol: u16) void {
        // 存储变量, 如果它是局部变量, 则初始化结果已经在正确的槽位上了, 即已完成.
        if (self.scope_depth >= 0) return;

        // 这是一个模块级变量, 所以将值存储在模块槽中, 然后丢弃初始化的临时值.
        self.emitShortArg(.CODE_STORE_MODULE_VAR, symbol);
        self.emitOp(.CODE_POP);
    }

    // 开启一个新的局部块作用域.
    fn pushScope(self: *@This()) void {
        self.scope_depth += 1;
    }

    // 生成代码以丢弃深度为[depth]或更大的局部变量.
    // 不过，*实际上并没有取消声明变量或弹出任何作用域.
    // 当编译"break"语句以在跳出循环之前丢弃局部变量时，即使它们仍在break指令的作用域内，也会直接调用此函数
    //
    // 返回被丢弃的局部变量的数量.
    fn discardLocals(self: *@This(), depth: i32) usize {
        Utils.assert(self.scope_depth > -1, "Should have a scope depth.");

        var local = self.num_locals - 1;
        while (local >= 0 and self.locals[local].depth >= depth) : (local -= 1) {
            // 如果局部变量被闭包了，确保当它从堆栈中超出作用域时，upvalue也会被关闭.
            // 这里使用emitByte()而不是emitOp(), 因为我们不想跟踪这些pops的堆栈效应，因为变量在中断后仍在作用域内。
            if (self.locals[local].is_upvalue) {
                _ = self.emitByte(OpCode.CODE_CLOSE_UPVALUE.num());
            } else {
                _ = self.emitByte(OpCode.CODE_POP.num());
            }
        }

        return self.num_locals - 1 - local;
    }

    // 关闭上一次push的块作用域并丢弃该作用域中声明的任何局部变量.
    // 该函数应当仅在堆栈上没有临时对象的语句上下文中调用.
    fn popScope(self: *@This()) void {
        const popped = self.discardLocals(self.scope_depth);
        self.num_locals -= popped;
        self.num_slots -= popped;
        self.scope_depth -= 1;
    }

    pub fn resolveLocal(self: *@This(), name: []const u8) ?usize {
        var i = self.num_locals;
        while (i > 0) {
            i -= 1;
            const lname = self.locals[i].name;
            if (lname.len == name.len and std.mem.eql(u8, name, lname)) return i;
        }
        return null;
    }

    pub fn addUpvalue(self: *@This(), is_local: bool, index: usize) usize {
        var func = self.func orelse @panic("Should have a function.");
        // 先查找是否存在
        for (0..func.num_upvalues) |i| {
            const upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) return i;
        }
        // 否则，添加一个新的upvalue
        self.upvalues[func.num_upvalues].is_local = is_local;
        self.upvalues[func.num_upvalues].index = index;
        func.num_upvalues += 1;
        return func.num_upvalues - 1;
    }

    // 尝试在由 [compiler] 编译的函数中查找 [name]
    // 如果找到，它会将一个upvalue添加到此编译器的upvalue列表中（除非它已经在那里）并返回其索引.
    // 没找到返回null.
    //
    // 如果在直接封闭函数之外找到该名称，这将展平闭包并将上值添加到所有中间函数，以便它向下移动到该函数.
    // 如果到达方法边界，则会停止并返回null，因为方法不会关闭局部变量
    pub fn findUpvalue(self: *@This(), name: []const u8) ?usize {
        // 如果当前位于顶层, 则不用查找
        const parent = self.parent orelse return null;

        // 如果到达了方法边界(并且不是静态字段), 则停止查找. 将会将其视为self send.
        if (name[0] != '_' and parent.enclosing_class != null) return null;

        // 检查其是否是直接封闭函数的局部变量.
        if (parent.resolveLocal(name)) |i| {
            parent.locals[i].is_upvalue = true; // 标记为upvalue, 以便在离开作用域时封闭.
            return self.addUpvalue(true, i);
        }
        // 检查其是否是立即封闭函数的upvalue.
        // 换句话说, 它是否是一个非立即封闭函数的局部变量.
        // 这会自动"扁平化"闭包: 它将upvalues添加到所有中间函数中, 以便从声明局部的函数一直到可能在其上关闭的深层嵌套函数中
        if (parent.findUpvalue(name)) |i| return self.addUpvalue(false, i);
        return null; // 遍历了所有父级, 仍未找到
    }

    pub fn resolveNonmodule(self: *@This(), name: []const u8) ?Variable {
        // 在局部作用域中查找
        if (self.resolveLocal(name)) |i| return .{ .scope = .SCOPE_LOCAL, .index = i };
        if (self.findUpvalue(name)) |i| return .{ .scope = .SCOPE_UPVALUE, .index = i };
        return null;
    }

    // 在当前作用域中查找[name]以查看它指的是哪个变量.
    // 如果在模块作用域, 局部作用域或封闭函数的upvalue列表中找到该名称, 则返回该变量.
    pub fn resolveName(self: *@This(), name: []const u8) Variable {
        if (self.resolveNonmodule(name)) |v| return v;
        const i = self.parser.vm.symbolTableFind(&self.parser.module.variable_names, name).?;
        return .{ .scope = .SCOPE_MODULE, .index = i };
    }

    pub fn signatureSymbol(self: *@This(), signature: *Signature) u16 {
        var name = [_]u8{0} ** C.MAX_METHOD_SIGNATURE;
        const fs = signature.toString(name[0..]);
        return @truncate(self.methodSymbol(fs));
    }

    // 返回一个带有 [type] 的签名，其名称来自最后使用的token
    pub fn signatureFromToken(self: *@This(), stype: SignatureType) Signature {
        var signature: Signature = .{};

        // 为方法名称获取token.
        const token = &self.parser.prev;
        signature.name = token.name();
        signature.stype = stype;
        signature.arity = 0;

        if (signature.name.len > C.MAX_METHOD_NAME) {
            self.doError("Method names cannot be longer than {d} characters.", .{C.MAX_METHOD_NAME});
            signature.name = signature.name[0..C.MAX_METHOD_NAME];
        }

        return signature;
    }

    // 解析逗号分隔的参数列表. 修改[signature]以包含参数列表的元数量
    pub fn finishArgumentList(self: *@This(), signature: *Signature) void {
        while (true) {
            self.ignoreNewLines();
            signature.arity += 1;
            self.validateNumParameters(signature.arity);
            self.expression();
            if (!self.match(.TOKEN_COMMA)) break;
        }
        self.ignoreNewLines(); // 允许在结束分隔符前有换行符.
    }

    // 根据签名和操作码编译method call.
    pub fn callSignature(self: *@This(), op: OpCode, signature: *Signature) void {
        const symbol = self.signatureSymbol(signature);
        self.emitShortArg(op.poffset(signature.arity), symbol);
        if (op == .CODE_SUPER_0) {
            // 需要静态绑定Super call到类的超类上.
            // 这样做可以确保即使一个方法包含一个super调用, 它也会被继承的子类调用正确的方法.
            //
            // 我们在类定义时通过在常量表中存储对超类的引用来绑定它.
            // 因此, 这里在常量表中创建一个槽并存储NULL.
            // 当方法被绑定时, 我们会在超类中查找它, 然后将其存储在该常量槽中.
            self.emitShort(@intCast(self.addConstant(.NULL_VAL) orelse std.math.maxInt(u16)));
        }
    }

    pub fn callMethod(self: *@This(), num_args: u8, name: []const u8) void {
        const symbol: u16 = @intCast(self.methodSymbol(name));
        self.emitShortArg(OpCode.CODE_CALL_0.poffset(num_args), symbol);
    }

    // 编译一个(可选)参数列表用于带有[methodSignature]的方法调用, 然后调用它.
    pub fn methodCall(self: *@This(), op: OpCode, signature: *Signature) void {
        // 基于我们找到的参数创建一个新的签名, 其中包含更新的元数量和类型.
        var called: Signature = .{
            .name = signature.name,
            .stype = .SIG_GETTER,
            .arity = 0,
        };

        if (self.match(.TOKEN_LEFT_PAREN)) {
            called.stype = .SIG_METHOD;
            self.ignoreNewLines(); // 允许在空参数列表前有换行符.
            // 允许空参数列表
            if (self.peek() != .TOKEN_RIGHT_PAREN) self.finishArgumentList(&called);
            self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
        }

        // 解析块参数, 如果有的话.
        if (self.match(.TOKEN_LEFT_BRACE)) {
            // 在arity中包含块参数.
            called.stype = .SIG_METHOD;
            called.arity += 1;

            var fnCompiler: Compiler = .{};
            fnCompiler.init(self.parser, self, false);

            // 构造一个假的签名来跟踪元数量.
            var fnSignature: Signature = .{
                .stype = .SIG_METHOD,
                .arity = 0,
            };

            // 解析参数列表, 如果有的话.
            if (self.match(.TOKEN_PIPE)) {
                fnCompiler.finishParameterList(&fnSignature);
                self.consume(.TOKEN_PIPE, "Expect '|' after function parameters.");
            }

            fnCompiler.func.?.arity = fnSignature.arity;
            fnCompiler.finishBody();

            // 根据传递给的方法来命名函数.
            var blockName = [_]u8{0} ** (C.MAX_METHOD_SIGNATURE + 15);
            const fs = called.toString(blockName[0..]);
            std.mem.copyForwards(u8, blockName[fs.len..], " block argument");

            _ = fnCompiler.endCompiler(blockName[0 .. fs.len + 15]);
        }

        // TODO Allow Grace-style mixfix methods?

        // 如果对于初始化器, 这是一个super()调用, 确保得到实际的参数列表.
        if (signature.stype == .SIG_INITIALIZER) {
            if (called.stype != .SIG_METHOD) {
                self.doError("A superclass constructor must have an argument list.", .{});
            }
            called.stype = .SIG_INITIALIZER;
        }

        self.callSignature(op, &called);
    }

    // 编译一个调用, 其名称是之前消费的token.
    // 其中包含getter, 带参方法调用和setter调用.
    pub fn namedCall(self: *@This(), can_assign: bool, op: OpCode) void {
        // 为方法名获取token
        var signature = self.signatureFromToken(.SIG_GETTER);

        if (can_assign and self.match(.TOKEN_EQ)) {
            self.ignoreNewLines();

            // 构建setter签名.
            signature.stype = .SIG_SETTER;
            signature.arity = 1;

            // 编译赋值value.
            self.expression();
            self.callSignature(op, &signature);
        } else {
            self.methodCall(op, &signature);
            self.allowLineBeforeDot();
        }
    }

    // 将上一个CODE_JUMP或CODE_JUMP_IF指令的占位符参数替换为跳转到当前字节码末尾的偏移量.
    pub fn patchJump(self: *@This(), offset: usize) void {
        // -2 用于调整跳转偏移量本身的字节码.
        const jump = self.func.?.code.count - offset - 2;
        if (jump > C.MAX_JUMP) self.doError("Too much code to jump over.", .{});

        self.func.?.code.rat(offset + 0).* = @truncate((jump >> 8) & 0xff);
        self.func.?.code.rat(offset + 1).* = @truncate((jump >> 0) & 0xff);
    }

    // 在初始的"{"被消费之后解析block.
    //
    // 如果是表达式block, 返回true. 如果是语句block, 返回false.
    // 更准确的说, 如果有值留在栈上, 返回true. 如果是空block, 返回false.
    pub fn finishBlock(self: *@This()) bool {
        // 空block, 什么也不做.
        if (self.match(.TOKEN_RIGHT_BRACE)) return false;

        // 如果在"{"之后没有行, 它是一个单表达式体.
        if (!self.matchLine()) {
            self.expression();
            self.consume(.TOKEN_RIGHT_BRACE, "Expect '}}' at end of block.");
            return true;
        }

        // 空block(其中只有换行)什么也不做.
        if (self.match(.TOKEN_RIGHT_BRACE)) return false;

        // 编译定义列表.
        while (true) {
            self.definition();
            self.consumeLine("Expect newline after statement.");
            if (self.peek() == .TOKEN_RIGHT_BRACE or self.peek() == .TOKEN_EOF) break;
        }

        self.consume(.TOKEN_RIGHT_BRACE, "Expect '}}' at end of block.");
        return false;
    }

    // 在初始的"{"被消费之后解析方法或函数体.
    //
    // 如果 [Compiler->is_initializer] 为 `true`, 这是构造函数初始化器的主体.
    // 这种情况下, 它添加了确保它返回`this`的代码.
    pub fn finishBody(self: *@This()) void {
        const isExpressionBody = self.finishBlock();

        if (self.is_initializer) {
            // 如果初始化器主体求值为一个value, 则丢弃.
            if (isExpressionBody) self.emitOp(.CODE_POP);

            // 接收者总是存储在第一个局部槽中.
            self.emitOp(.CODE_LOAD_LOCAL_0);
        } else if (!isExpressionBody) {
            self.emitOp(.CODE_NULL); // 语句体中隐式返回null.
        }

        self.emitOp(.CODE_RETURN);
    }

    // VM仅能处理一定数量的参数, 检查是否超限, 并给出错误信息.
    pub fn validateNumParameters(self: *@This(), num_args: u8) void {
        if (num_args == C.MAX_PARAMETERS + 1) {
            // 仅当参数正好是最大值加一时才显示错误, 这样可以继续解析参数并减少级联错误.
            self.doError("Methods cannot have more than {d} parameters.", .{C.MAX_PARAMETERS});
        }
    }

    pub fn finishParameterList(self: *@This(), signature: *Signature) void {
        while (true) {
            self.ignoreNewLines();
            signature.arity += 1;
            self.validateNumParameters(signature.arity);

            // 为参数定义一个方法中的局部变量.
            _ = self.declareNamedVariable();
            if (!self.match(.TOKEN_COMMA)) break;
        }
    }

    pub fn methodSymbol(self: *@This(), name: []const u8) usize {
        return self.parser.vm.symbolTableEnsure(&self.parser.vm.method_names, name);
    }

    // 在方法 [signature] 中编译一个可选的setter参数.
    // 如果是setter, 返回true.
    pub fn maybeSetter(self: *@This(), signature: *Signature) bool {
        // 检查是否是settter
        if (!self.match(.TOKEN_EQ)) return false;

        signature.stype = if (signature.stype == .SIG_SUBSCRIPT) .SIG_SUBSCRIPT_SETTER else .SIG_SETTER;

        // 解析value参数.
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after '='.");
        _ = self.declareNamedVariable();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");

        signature.arity += 1;

        return true;
    }

    // 遍历编译器链来查找包含该类的最近的类的编译器.
    // 如果不在类定义中, 返回NULL.
    pub fn getEnclosingClassCompiler(self: *@This()) ?*Compiler {
        var compiler: ?*Compiler = self;
        while (compiler) |c| : (compiler = c.parent) {
            if (c.enclosing_class != null) return compiler;
        }
        return null;
    }

    // 遍历编译器链来查找包含该类的最近的类.
    // 如果不在类定义中, 返回NULL.
    pub fn getEnclosingClass(self: *@This()) ?*ClassInfo {
        const c = self.getEnclosingClassCompiler() orelse return null;
        return c.enclosing_class;
    }

    // 编译对于 [variable] 的读取或赋值指令.
    pub fn bareName(self: *@This(), can_assign: bool, variable: Variable) void {
        // 如果在裸变量名后面有一个 "=" 则对应的是一个变量赋值.
        if (can_assign and self.match(.TOKEN_EQ)) {
            self.expression(); // 编译右边的表达式
            const arg: u8 = @intCast(variable.index); // TODO 类型改进
            // 编译存储指令.
            switch (variable.scope) {
                .SCOPE_LOCAL => _ = self.emitByteArg(.CODE_STORE_LOCAL, arg),
                .SCOPE_UPVALUE => _ = self.emitByteArg(.CODE_STORE_UPVALUE, arg),
                .SCOPE_MODULE => _ = self.emitShortArg(.CODE_STORE_MODULE_VAR, arg),
            }
            return;
        }

        self.loadVariable(variable); // 编译加载指令
        self.allowLineBeforeDot();
    }

    // 解析一个可选的括号参数列表. 根据解析的内容更新 [signature] 中的 `type` 和 `arity`.
    pub fn parameterList(self: *@This(), signature: *Signature) void {
        // 参数列表是可选的.
        if (!self.match(.TOKEN_LEFT_PAREN)) return;

        signature.stype = .SIG_METHOD;

        self.ignoreNewLines(); // 在空参数列表之前允许换行符.

        // 允许空参数列表.
        if (self.match(.TOKEN_RIGHT_PAREN)) return;

        self.finishParameterList(signature);
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    }

    pub fn null_(self: *@This(), can_assign: bool) void {
        _ = can_assign;
        self.emitOp(.CODE_NULL);
    }

    pub fn literal(self: *Compiler, can_assign: bool) void {
        _ = can_assign;
        self.emitConstant(self.parser.prev.value);
    }

    // 编译变量名或带有隐式接收者的方法调用.
    pub fn parseName(self: *Compiler, can_assign: bool) void {
        // 在最近的封闭方法的作用域链中查找名称.
        const token = &self.parser.prev;

        if (self.resolveNonmodule(token.name())) |v| {
            self.bareName(can_assign, v);
            return;
        }

        // TODO The fact that we return above here if the variable is known and parse an optional argument list below if not means that the grammar is not context-free.
        // A line of code in a method like "someName(foo)" is a parse error if "someName" is a defined variable in the surrounding scope and not if it isn't. Fix this.
        // One option is to have "someName(foo)" always resolve to a self-call if there is an argument list, but that makes getters a little confusing.

        // 如果在方法中, 并且名称是小写的, 则将其视为this的方法.
        if (Utils.isLocalName(token.name()[0]) and self.getEnclosingClass() != null) {
            self.loadThis();
            self.namedCall(can_assign, .CODE_CALL_0);
            return;
        }

        // 否则, 在模块级别查找名称.
        var vm = self.parser.vm;
        var variable: Variable = .{ .index = 0, .scope = .SCOPE_MODULE };
        if (vm.symbolTableFind(&self.parser.module.variable_names, token.name())) |i| {
            variable.index = i;
        } else {
            // 隐式定义一个模块级变量, 希望稍后得到一个真正的定义.
            variable.index = vm.declareVariable(self.parser.module, token.name(), token.line) orelse {
                self.doError("Too many module variables defined.", .{});
                return;
            };
        }

        self.bareName(can_assign, variable);
    }

    fn forStatement(self: *@This()) void {
        // 一个 for 语句如下所示:
        //
        //     for (i in sequence.expression) {
        //       System.print(i)
        //     }
        //
        // 其被编译成字节码, 大概看起来像这样:
        //
        //     {
        //       var seq_ = sequence.expression
        //       var iter_
        //       while (iter_ = seq_.iterate(iter_)) {
        //         var i = seq_.iteratorValue(iter_)
        //         System.print(i)
        //       }
        //     }
        //
        // 事实并非完全如此，因为合成变量"seq_"和"iter_"等名并非合法标识符，但这是基本思路
        //
        // 重要的部分是:
        // - sequence表达式只会被求值一次.
        // - .iterate()方法被用于向前移动迭代器并确定是否应该退出循环.
        // - .iteratorValue()方法用于获取当前迭代器位置的值.

        // 对用于迭代器的隐藏局部变量创建一个作用域.
        self.pushScope();

        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
        self.consume(.TOKEN_NAME, "Expect for loop variable name.");

        // 记录循环变量的名称.
        const name = self.parser.prev.name();

        self.consume(.TOKEN_IN, "Expect 'in' after loop variable.");
        self.ignoreNewLines();

        // 对sequence表达式求值, 并存储在隐藏的局部变量中.
        // 后续变量名中的空格确保它不会与用户定义的变量冲突.
        self.expression();

        // 验证是否有足够的空间来存储隐藏的局部变量.
        // 我们期望在下面的代码中连续调用两个addLocal.
        if (self.num_locals + 2 > C.MAX_LOCALS) {
            self.doError("Cannot declare more than {d} variables in one scope. (Not enough space for for-loops internal variables)", .{C.MAX_LOCALS});
            return;
        }
        const seq_slot: u8 = @intCast(self.addLocal("seq "));

        // 创建另外一个隐藏局部变量 用于迭代器对象.
        self.null_(false);
        const iter_slot: u8 = @intCast(self.addLocal("iter "));

        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after loop expression.");

        var loop: Loop = .{};
        self.startLoop(&loop);

        // 通过调用.iterate方法在序列上前移迭代器.
        self.loadLocal(seq_slot);
        self.loadLocal(iter_slot);

        // 更新并测试迭代器.
        self.callMethod(1, "iterate(_)");
        _ = self.emitByteArg(.CODE_STORE_LOCAL, iter_slot);
        self.testExitLoop();

        // 通过调用.iteratorValue获取序列中的当前值.
        self.loadLocal(seq_slot);
        self.loadLocal(iter_slot);
        self.callMethod(1, "iteratorValue(_)");

        // 在其自己的作用域中绑定循环变量.
        // 确保每个循环迭代都得到一个新的变量, 这样闭包就不会看到同一个变量.
        self.pushScope();
        _ = self.addLocal(name);
        self.loopBody();
        self.popScope(); // 循环变量
        self.endLoop();
        self.popScope(); // 隐藏变量
    }

    fn ifStatement(self: *@This()) void {
        // 编译condition
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
        self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after if condition.");

        // 当condition为false时, 跳转到else分支.
        const if_jump = self.emitJump(.CODE_JUMP_IF);

        self.statement(); // 编译then分支

        // 如果有else分支, 则编译它.
        if (self.match(.TOKEN_ELSE)) {
            // 当if分支被采用时, 跳过else分支.
            const else_jump = self.emitJump(.CODE_JUMP);
            self.patchJump(if_jump);

            self.statement();

            self.patchJump(else_jump); // 填充elseJump
        } else {
            self.patchJump(if_jump);
        }
    }

    fn whileStatement(self: *@This()) void {
        var loop: Loop = undefined;
        self.startLoop(&loop);

        // 编译condition
        self.consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
        self.expression();
        self.consume(.TOKEN_RIGHT_PAREN, "Expect ')' after while condition.");

        self.testExitLoop();
        self.loopBody();
        self.endLoop();
    }
    // 编译简单的语句. 这些只能出现在顶层或位于大括号block中.
    // 简单语句不包括变量绑定语句, 如"var"和"class", 这些语句不允许直接出现在"if"语句的分支等位置.
    // 和表达式不同, 语句不会在栈上留下值.
    fn statement(self: *@This()) void {
        if (self.match(.TOKEN_BREAK)) {
            if (self.loop) |loop| {
                // 因为将跳出作用域, 所以要确保作用域中的任何局部变量都被丢弃.
                _ = self.discardLocals(loop.scope_depth + 1);

                // 为跳转到循环体末尾生成占位符指令.
                // 当完成循环体编译并知道结束位置时, 我们会用合适的偏移量替换这些.
                // 这里我们使用`CODE_END`是因为它不能出现在字节码的中间.
                _ = self.emitJump(.CODE_END);
            } else {
                self.doError("Cannot use 'break' outside of a loop.", .{});
                return;
            }
        } else if (self.match(.TOKEN_CONTINUE)) {
            if (self.loop) |loop| {
                // 因为将跳出作用域, 所以要确保作用域中的任何局部变量都被丢弃.
                _ = self.discardLocals(loop.scope_depth + 1);

                // 为跳转到循环体开头生成占位符指令.
                const loop_offset: u16 = @intCast(self.func.?.code.count - loop.start + 3);
                self.emitShortArg(.CODE_LOOP, loop_offset);
            } else {
                self.doError("Cannot use 'continue' outside of a loop.", .{});
                return;
            }
        } else if (self.match(.TOKEN_FOR)) {
            self.forStatement();
        } else if (self.match(.TOKEN_IF)) {
            self.ifStatement();
        } else if (self.match(.TOKEN_RETURN)) {
            // 编译返回值.
            if (self.peek() == .TOKEN_LINE) {
                // 如果在return后没有表达式, 初始化器应返回'this', 而常规方法应返回null.
                const result: OpCode = if (self.is_initializer) .CODE_LOAD_LOCAL_0 else .CODE_NULL;
                self.emitOp(result);
            } else {
                if (self.is_initializer) self.doError("A constructor cannot return a value.", .{});
                self.expression();
            }

            self.emitOp(.CODE_RETURN);
        } else if (self.match(.TOKEN_WHILE)) {
            self.whileStatement();
        } else if (self.match(.TOKEN_LEFT_BRACE)) {
            // 块语句
            self.pushScope();
            if (self.finishBlock()) self.emitOp(.CODE_POP); // 块是表达式, 因此丢弃.
            self.popScope();
        } else {
            self.expression(); // 表达式语句
            self.emitOp(.CODE_POP);
        }
    }
};

pub const Local = struct {
    name: []const u8 = &.{}, // 局部变量名, 直接指向源代码字符串
    depth: i32 = 0, // 该变量所在的scope深度, 0代表最外层作用域--方法的参数, 或顶层代码中的第一个局部块
    is_upvalue: bool = false, // 当前局部变量是否被用于upvalue
};

pub const Loop = struct {
    start: usize = 0, // 该loop应当转跳回的指令索引
    exit_jump: usize = 0, // 用于跳出该loop的`CODE_JUMP_IF`指令的argument索引
    body: usize = 0, // 循环体的起始指令索引
    scope_depth: i32 = 0, // 当loop中的一个break命中时, 应当退出的scope深度
    enclosing: ?*@This() = null, // 为NULL代表该loop是最外层的loop,否则指向包围该loop的loop
};

pub const ClassInfo = struct {
    name: *ObjString = undefined, // 类名
    class_attrs: ?*ObjMap = null, // 类自身的属性
    method_attrs: ?*ObjMap = null, // 类的方法的属性
    fields: SymbolTable = undefined, // 类的字段符号表
    methods: UsizeBuffer = undefined, // 类中定义的方法的符号索引. 用于检测重复定义的方法
    static_methods: UsizeBuffer = undefined,
    is_foreign: bool = undefined, // 为True时当前被编译的类是foreign类
    in_static: bool = undefined, // 为True时当前被编译的类是static类
    signature: *Signature = undefined, // 被编译的方法的签名
};

pub const CompilerUpvalue = struct {
    is_local: bool = false, // 为True代表封闭函数中的upvalue捕获了局部变量, False代表捕获了upvalue
    index: usize = 0, // 被捕获到封闭函数中的局部或upvalue变量的索引
};

pub const Token = struct {
    token_type: TokenType = .TOKEN_NONE,
    source_ref: []const u8 = &.{}, // 源码引用
    char_start: usize = 0, // 代表token的起始位置(在source中的位置)
    char_num: usize = 0, //   当前token的名称长度.
    line: usize = 1, //       当前token对应行号, 从1开始.
    value: Value = .{}, //    当token是字面量时对应的value.

    pub fn name(self: @This()) []const u8 {
        return self.source_ref[self.char_start .. self.char_start + self.char_num];
    }
};

// 描述变量声明的位置
pub const Scope = enum {
    SCOPE_LOCAL, //   当前函数中的局部变量.
    SCOPE_UPVALUE, // 封闭函数中定义的局部变量.
    SCOPE_MODULE, //  顶层模块变量.
};

// 对变量的引用和定义它的作用域.
// 包含了足够的信息用于生成正确的代码用于加载或存储变量.
pub const Variable = struct {
    index: usize = 0, // 栈槽位, upvalue槽位, 或定义该变量的模块符号
    scope: Scope = undefined, // 定义变量的作用域.
};

const Keyword = struct {
    identifier: []const u8,
    token_type: TokenType,
};

const TokenType = enum(u8) {
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACKET,
    TOKEN_RIGHT_BRACKET,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COLON,
    TOKEN_DOT,
    TOKEN_DOTDOT,
    TOKEN_DOTDOTDOT,
    TOKEN_COMMA,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_PERCENT,
    TOKEN_HASH,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_LTLT,
    TOKEN_GTGT,
    TOKEN_PIPE,
    TOKEN_PIPEPIPE,
    TOKEN_CARET,
    TOKEN_AMP,
    TOKEN_AMPAMP,
    TOKEN_BANG,
    TOKEN_TILDE,
    TOKEN_QUESTION,
    TOKEN_EQ,
    TOKEN_LT,
    TOKEN_GT,
    TOKEN_LTEQ,
    TOKEN_GTEQ,
    TOKEN_EQEQ,
    TOKEN_BANGEQ,

    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_CLASS,
    TOKEN_CONSTRUCT,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FOREIGN,
    TOKEN_IF,
    TOKEN_IMPORT,
    TOKEN_AS,
    TOKEN_IN,
    TOKEN_IS,
    TOKEN_NULL,
    TOKEN_RETURN,
    TOKEN_STATIC,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_FIELD,
    TOKEN_STATIC_FIELD,
    TOKEN_NAME,
    TOKEN_NUMBER,

    // 没有插值的字符串字面量, 或最后一个插值表达式后的字符串部分.
    TOKEN_STRING,

    // 插值表达式前的字符串部分. 对于字符串:
    //     "a %(b) c %(d) e"
    // 会被解析为:
    //     TOKEN_INTERPOLATION "a "
    //     TOKEN_NAME          b
    //     TOKEN_INTERPOLATION " c "
    //     TOKEN_NAME          d
    //     TOKEN_STRING        " e"
    TOKEN_INTERPOLATION,

    TOKEN_LINE,

    TOKEN_ERROR,
    TOKEN_EOF,
    TOKEN_NONE,

    pub inline fn num(self: @This()) u8 {
        return @intFromEnum(self);
    }
};
