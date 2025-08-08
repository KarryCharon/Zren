const OpCode = @import("opcode.zig").OpCode;

pub const C = struct {
    // 单次可定义的模块级最大变量数.
    // 该限制源自字节码 `LOAD_MODULE_VAR` 和 `STORE_MODULE_VAR` 使用两字节参数.
    pub const MAX_MODULE_VARS: u32 = 65536;

    // 可以传递给函数的最大参数数. 该限制是硬编码的, 参见字节码 `CALL_XX` 等
    pub const MAX_PARAMETERS: u8 = 16;

    // 方法名称的最大长度, 不包括签名. 虽然可以是任意的, 但强制限制, 以确保解析器中的方法名称字符串需要多长.
    pub const MAX_METHOD_NAME: u8 = 64;

    // 方法签名的最大长度. 签名大概长这样:
    //
    //     foo        // Getter.
    //     foo()      // 无参方法
    //     foo(_)     // 1个参数方法.
    //     foo(_,_)   // 2个参数方法.
    //     init foo() // 构造函数初始化器
    //
    // 最大签名长度考虑了最长方法名, 最大参数数(带分隔符), "init ", 和 "()".
    pub const MAX_METHOD_SIGNATURE: u16 = MAX_METHOD_NAME + (MAX_PARAMETERS * 2) + 6;

    // 最大长度的标识符. 该限制的真正原因是为了错误消息中涉及的变量内容可以栈分配
    pub const MAX_VARIABLE_NAME: u8 = 64;

    // 类可以拥有的最大字段数, 包括继承的字段.
    // 该限制源字节码 `CLASS` 和 `SUBCLASS` 使用一个字节来表示字段数.
    pub const MAX_FIELDS: u8 = 255;

    // 单个函数、方法或顶级代码块中可声明的最大局部变量（即非模块级）.
    // 也即同时处于作用域中的变量数量(包括块作用域).
    // 该限制源自字节码 `LOAD_LOCAL` 和 `STORE_LOCAL` 使用一个字节来表示局部变量.
    pub const MAX_LOCALS: u16 = 256;

    // 函数可以拥有的最大闭包数量.
    pub const MAX_UPVALUES: u16 = 256;

    // 函数可以拥有的最大不同常量数量. 该限制源自字节码 `CONSTANT` 使用两个字节表示变量.
    pub const MAX_CONSTANTS: u32 = 1 << 16;

    // 字节码 `JUMP` 和 `JUMP_IF` 可以跳转的最大偏移量.
    pub const MAX_JUMP: u32 = 1 << 16;

    // 插值可以嵌套的最大深度. 例如, 这个字符串有3层嵌套:
    //      "outside %(one + "%(two + "%(three)")")"
    pub const MAX_INTERPOLATION_NESTING: u8 = 8;

    // 用于格式化编译错误消息的缓冲区大小, 不包括模块名称和错误位置的信息头.
    // 使用硬编码缓冲区大小有点麻烦, 幸运的是, 我们可以控制并处理可能的最长消息长度.
    pub const ERROR_MESSAGE_SIZE: u8 = 80 + MAX_VARIABLE_NAME + 15;

    pub const MAX_TEMP_ROOTS: u8 = 8;
    pub const INITIAL_CALL_FRAMES: u8 = 4;

    // 用于二进制wren的退出码. 遵循BSD标准.
    pub const EX_USAGE: u8 = 64; //     解释器被传入错误数量的参数
    pub const EX_DATAERR: u8 = 65; //   编译错误
    pub const EX_NOINPUT: u8 = 66; //   无法打开输入文件
    pub const EX_SOFTWARE: u8 = 70; //  运行时错误
    pub const EX_IOERR: u8 = 74; //     I/O 错误

    // 字节码的对应的栈槽数量. 该数组的索引是OpCode, 值是OpCode对应的栈槽数量.
    pub const StackEffects = &[_]struct { code: OpCode, effect: isize }{
        .{ .code = .CODE_CONSTANT, .effect = 1 },
        .{ .code = .CODE_NULL, .effect = 1 },
        .{ .code = .CODE_FALSE, .effect = 1 },
        .{ .code = .CODE_TRUE, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_0, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_1, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_2, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_3, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_4, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_5, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_6, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_7, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL_8, .effect = 1 },
        .{ .code = .CODE_LOAD_LOCAL, .effect = 1 },
        .{ .code = .CODE_STORE_LOCAL, .effect = 0 },
        .{ .code = .CODE_LOAD_UPVALUE, .effect = 1 },
        .{ .code = .CODE_STORE_UPVALUE, .effect = 0 },
        .{ .code = .CODE_LOAD_MODULE_VAR, .effect = 1 },
        .{ .code = .CODE_STORE_MODULE_VAR, .effect = 0 },
        .{ .code = .CODE_LOAD_FIELD_THIS, .effect = 1 },
        .{ .code = .CODE_STORE_FIELD_THIS, .effect = 0 },
        .{ .code = .CODE_LOAD_FIELD, .effect = 0 },
        .{ .code = .CODE_STORE_FIELD, .effect = -1 },
        .{ .code = .CODE_POP, .effect = -1 },
        .{ .code = .CODE_CALL_0, .effect = 0 },
        .{ .code = .CODE_CALL_1, .effect = -1 },
        .{ .code = .CODE_CALL_2, .effect = -2 },
        .{ .code = .CODE_CALL_3, .effect = -3 },
        .{ .code = .CODE_CALL_4, .effect = -4 },
        .{ .code = .CODE_CALL_5, .effect = -5 },
        .{ .code = .CODE_CALL_6, .effect = -6 },
        .{ .code = .CODE_CALL_7, .effect = -7 },
        .{ .code = .CODE_CALL_8, .effect = -8 },
        .{ .code = .CODE_CALL_9, .effect = -9 },
        .{ .code = .CODE_CALL_10, .effect = -10 },
        .{ .code = .CODE_CALL_11, .effect = -11 },
        .{ .code = .CODE_CALL_12, .effect = -12 },
        .{ .code = .CODE_CALL_13, .effect = -13 },
        .{ .code = .CODE_CALL_14, .effect = -14 },
        .{ .code = .CODE_CALL_15, .effect = -15 },
        .{ .code = .CODE_CALL_16, .effect = -16 },
        .{ .code = .CODE_SUPER_0, .effect = 0 },
        .{ .code = .CODE_SUPER_1, .effect = -1 },
        .{ .code = .CODE_SUPER_2, .effect = -2 },
        .{ .code = .CODE_SUPER_3, .effect = -3 },
        .{ .code = .CODE_SUPER_4, .effect = -4 },
        .{ .code = .CODE_SUPER_5, .effect = -5 },
        .{ .code = .CODE_SUPER_6, .effect = -6 },
        .{ .code = .CODE_SUPER_7, .effect = -7 },
        .{ .code = .CODE_SUPER_8, .effect = -8 },
        .{ .code = .CODE_SUPER_9, .effect = -9 },
        .{ .code = .CODE_SUPER_10, .effect = -10 },
        .{ .code = .CODE_SUPER_11, .effect = -11 },
        .{ .code = .CODE_SUPER_12, .effect = -12 },
        .{ .code = .CODE_SUPER_13, .effect = -13 },
        .{ .code = .CODE_SUPER_14, .effect = -14 },
        .{ .code = .CODE_SUPER_15, .effect = -15 },
        .{ .code = .CODE_SUPER_16, .effect = -16 },
        .{ .code = .CODE_JUMP, .effect = 0 },
        .{ .code = .CODE_LOOP, .effect = 0 },
        .{ .code = .CODE_JUMP_IF, .effect = -1 },
        .{ .code = .CODE_AND, .effect = -1 },
        .{ .code = .CODE_OR, .effect = -1 },
        .{ .code = .CODE_CLOSE_UPVALUE, .effect = -1 },
        .{ .code = .CODE_RETURN, .effect = 0 },
        .{ .code = .CODE_CLOSURE, .effect = 1 },
        .{ .code = .CODE_CONSTRUCT, .effect = 0 },
        .{ .code = .CODE_FOREIGN_CONSTRUCT, .effect = 0 },
        .{ .code = .CODE_CLASS, .effect = -1 },
        .{ .code = .CODE_END_CLASS, .effect = -2 },
        .{ .code = .CODE_FOREIGN_CLASS, .effect = -1 },
        .{ .code = .CODE_METHOD_INSTANCE, .effect = -2 },
        .{ .code = .CODE_METHOD_STATIC, .effect = -2 },
        .{ .code = .CODE_END_MODULE, .effect = 1 },
        .{ .code = .CODE_IMPORT_MODULE, .effect = 1 },
        .{ .code = .CODE_IMPORT_VARIABLE, .effect = 1 },
        .{ .code = .CODE_END, .effect = 0 },
    };
};

pub const coreModuleSource: []const u8 =
    \\class Bool {}
    \\class Fiber {}
    \\class Fn {}
    \\class Null {}
    \\class Num {}
    \\
    \\class Sequence {
    \\  all(f) {
    \\    var result = true
    \\    for (element in this) {
    \\      result = f.call(element)
    \\      if (!result) return result
    \\    }
    \\    return result
    \\  }
    \\
    \\  any(f) {
    \\    var result = false
    \\    for (element in this) {
    \\      result = f.call(element)
    \\      if (result) return result
    \\    }
    \\    return result
    \\  }
    \\
    \\  contains(element) {
    \\    for (item in this) {
    \\      if (element == item) return true
    \\    }
    \\    return false
    \\  }
    \\
    \\  count {
    \\    var result = 0
    \\    for (element in this) {
    \\      result = result + 1
    \\    }
    \\    return result
    \\  }
    \\
    \\  count(f) {
    \\    var result = 0
    \\    for (element in this) {
    \\      if (f.call(element)) result = result + 1
    \\    }
    \\    return result
    \\  }
    \\
    \\  each(f) {
    \\    for (element in this) {
    \\      f.call(element)
    \\    }
    \\  }
    \\
    \\  isEmpty { iterate(null) ? false : true }
    \\
    \\  map(transformation) { MapSequence.new(this, transformation) }
    \\
    \\  skip(count) {
    \\    if (!(count is Num) || !count.isInteger || count < 0) {
    \\      Fiber.abort("Count must be a non-negative integer.")
    \\    }
    \\
    \\    return SkipSequence.new(this, count)
    \\  }
    \\
    \\  take(count) {
    \\    if (!(count is Num) || !count.isInteger || count < 0) {
    \\      Fiber.abort("Count must be a non-negative integer.")
    \\    }
    \\
    \\    return TakeSequence.new(this, count)
    \\  }
    \\
    \\  where(predicate) { WhereSequence.new(this, predicate) }
    \\
    \\  reduce(acc, f) {
    \\    for (element in this) {
    \\      acc = f.call(acc, element)
    \\    }
    \\    return acc
    \\  }
    \\
    \\  reduce(f) {
    \\    var iter = iterate(null)
    \\    if (!iter) Fiber.abort("Can't reduce an empty sequence.")
    \\
    \\    // Seed with the first element.
    \\    var result = iteratorValue(iter)
    \\    while (iter = iterate(iter)) {
    \\      result = f.call(result, iteratorValue(iter))
    \\    }
    \\
    \\    return result
    \\  }
    \\
    \\  join() { join("") }
    \\
    \\  join(sep) {
    \\    var first = true
    \\    var result = ""
    \\
    \\    for (element in this) {
    \\      if (!first) result = result + sep
    \\      first = false
    \\      result = result + element.toString
    \\    }
    \\
    \\    return result
    \\  }
    \\
    \\  toList {
    \\    var result = List.new()
    \\    for (element in this) {
    \\      result.add(element)
    \\    }
    \\    return result
    \\  }
    \\}
    \\
    \\class MapSequence is Sequence {
    \\  construct new(sequence, fn) {
    \\    _sequence = sequence
    \\    _fn = fn
    \\  }
    \\
    \\  iterate(iterator) { _sequence.iterate(iterator) }
    \\  iteratorValue(iterator) { _fn.call(_sequence.iteratorValue(iterator)) }
    \\}
    \\
    \\class SkipSequence is Sequence {
    \\  construct new(sequence, count) {
    \\    _sequence = sequence
    \\    _count = count
    \\  }
    \\
    \\  iterate(iterator) {
    \\    if (iterator) {
    \\      return _sequence.iterate(iterator)
    \\    } else {
    \\      iterator = _sequence.iterate(iterator)
    \\      var count = _count
    \\      while (count > 0 && iterator) {
    \\        iterator = _sequence.iterate(iterator)
    \\        count = count - 1
    \\      }
    \\      return iterator
    \\    }
    \\  }
    \\
    \\  iteratorValue(iterator) { _sequence.iteratorValue(iterator) }
    \\}
    \\
    \\class TakeSequence is Sequence {
    \\  construct new(sequence, count) {
    \\    _sequence = sequence
    \\    _count = count
    \\  }
    \\
    \\  iterate(iterator) {
    \\    if (!iterator) _taken = 1 else _taken = _taken + 1
    \\    return _taken > _count ? null : _sequence.iterate(iterator)
    \\  }
    \\
    \\  iteratorValue(iterator) { _sequence.iteratorValue(iterator) }
    \\}
    \\
    \\class WhereSequence is Sequence {
    \\  construct new(sequence, fn) {
    \\    _sequence = sequence
    \\    _fn = fn
    \\  }
    \\
    \\  iterate(iterator) {
    \\    while (iterator = _sequence.iterate(iterator)) {
    \\      if (_fn.call(_sequence.iteratorValue(iterator))) break
    \\    }
    \\    return iterator
    \\  }
    \\
    \\  iteratorValue(iterator) { _sequence.iteratorValue(iterator) }
    \\}
    \\
    \\class String is Sequence {
    \\  bytes { StringByteSequence.new(this) }
    \\  codePoints { StringCodePointSequence.new(this) }
    \\
    \\  split(delimiter) {
    \\    if (!(delimiter is String) || delimiter.isEmpty) {
    \\      Fiber.abort("Delimiter must be a non-empty string.")
    \\    }
    \\
    \\    var result = []
    \\
    \\    var last = 0
    \\    var index = 0
    \\
    \\    var delimSize = delimiter.byteCount_
    \\    var size = byteCount_
    \\
    \\    while (last < size && (index = indexOf(delimiter, last)) != -1) {
    \\      result.add(this[last...index])
    \\      last = index + delimSize
    \\    }
    \\
    \\    if (last < size) {
    \\      result.add(this[last..-1])
    \\    } else {
    \\      result.add("")
    \\    }
    \\    return result
    \\  }
    \\
    \\  replace(from, to) {
    \\    if (!(from is String) || from.isEmpty) {
    \\      Fiber.abort("From must be a non-empty string.")
    \\    } else if (!(to is String)) {
    \\      Fiber.abort("To must be a string.")
    \\    }
    \\
    \\    var result = ""
    \\
    \\    var last = 0
    \\    var index = 0
    \\
    \\    var fromSize = from.byteCount_
    \\    var size = byteCount_
    \\
    \\    while (last < size && (index = indexOf(from, last)) != -1) {
    \\      result = result + this[last...index] + to
    \\      last = index + fromSize
    \\    }
    \\
    \\    if (last < size) result = result + this[last..-1]
    \\
    \\    return result
    \\  }
    \\
    \\  trim() { trim_("\t\r\n ", true, true) }
    \\  trim(chars) { trim_(chars, true, true) }
    \\  trimEnd() { trim_("\t\r\n ", false, true) }
    \\  trimEnd(chars) { trim_(chars, false, true) }
    \\  trimStart() { trim_("\t\r\n ", true, false) }
    \\  trimStart(chars) { trim_(chars, true, false) }
    \\
    \\  trim_(chars, trimStart, trimEnd) {
    \\    if (!(chars is String)) {
    \\      Fiber.abort("Characters must be a string.")
    \\    }
    \\
    \\    var codePoints = chars.codePoints.toList
    \\
    \\    var start
    \\    if (trimStart) {
    \\      while (start = iterate(start)) {
    \\        if (!codePoints.contains(codePointAt_(start))) break
    \\      }
    \\
    \\      if (start == false) return ""
    \\    } else {
    \\      start = 0
    \\    }
    \\
    \\    var end
    \\    if (trimEnd) {
    \\      end = byteCount_ - 1
    \\      while (end >= start) {
    \\        var codePoint = codePointAt_(end)
    \\        if (codePoint != -1 && !codePoints.contains(codePoint)) break
    \\        end = end - 1
    \\      }
    \\
    \\      if (end < start) return ""
    \\    } else {
    \\      end = -1
    \\    }
    \\
    \\    return this[start..end]
    \\  }
    \\
    \\  *(count) {
    \\    if (!(count is Num) || !count.isInteger || count < 0) {
    \\      Fiber.abort("Count must be a non-negative integer.")
    \\    }
    \\
    \\    var result = ""
    \\    for (i in 0...count) {
    \\      result = result + this
    \\    }
    \\    return result
    \\  }
    \\}
    \\
    \\class StringByteSequence is Sequence {
    \\  construct new(string) {
    \\    _string = string
    \\  }
    \\
    \\  [index] { _string.byteAt_(index) }
    \\  iterate(iterator) { _string.iterateByte_(iterator) }
    \\  iteratorValue(iterator) { _string.byteAt_(iterator) }
    \\
    \\  count { _string.byteCount_ }
    \\}
    \\
    \\class StringCodePointSequence is Sequence {
    \\  construct new(string) {
    \\    _string = string
    \\  }
    \\
    \\  [index] { _string.codePointAt_(index) }
    \\  iterate(iterator) { _string.iterate(iterator) }
    \\  iteratorValue(iterator) { _string.codePointAt_(iterator) }
    \\
    \\  count { _string.count }
    \\}
    \\
    \\class List is Sequence {
    \\  addAll(other) {
    \\    for (element in other) {
    \\      add(element)
    \\    }
    \\    return other
    \\  }
    \\
    \\  sort() { sort {|low, high| low < high } }
    \\
    \\  sort(comparer) {
    \\    if (!(comparer is Fn)) {
    \\      Fiber.abort("Comparer must be a function.")
    \\    }
    \\    quicksort_(0, count - 1, comparer)
    \\    return this
    \\  }
    \\
    \\  quicksort_(low, high, comparer) {
    \\    if (low < high) {
    \\      var p = partition_(low, high, comparer)
    \\      quicksort_(low, p - 1, comparer)
    \\      quicksort_(p + 1, high, comparer)
    \\    }
    \\  }
    \\
    \\  partition_(low, high, comparer) {
    \\    var p = this[high]
    \\    var i = low - 1
    \\    for (j in low..(high-1)) {
    \\      if (comparer.call(this[j], p)) {  
    \\        i = i + 1
    \\        var t = this[i]
    \\        this[i] = this[j]
    \\        this[j] = t
    \\      }
    \\    }
    \\    var t = this[i+1]
    \\    this[i+1] = this[high]
    \\    this[high] = t
    \\    return i+1
    \\  }
    \\
    \\  toString { "[%(join(", "))]" }
    \\
    \\  +(other) {
    \\    var result = this[0..-1]
    \\    for (element in other) {
    \\      result.add(element)
    \\    }
    \\    return result
    \\  }
    \\
    \\  *(count) {
    \\    if (!(count is Num) || !count.isInteger || count < 0) {
    \\      Fiber.abort("Count must be a non-negative integer.")
    \\    }
    \\
    \\    var result = []
    \\    for (i in 0...count) {
    \\      result.addAll(this)
    \\    }
    \\    return result
    \\  }
    \\}
    \\
    \\class Map is Sequence {
    \\  keys { MapKeySequence.new(this) }
    \\  values { MapValueSequence.new(this) }
    \\
    \\  toString {
    \\    var first = true
    \\    var result = "{"
    \\
    \\    for (key in keys) {
    \\      if (!first) result = result + ", "
    \\      first = false
    \\      result = result + "%(key): %(this[key])"
    \\    }
    \\
    \\    return result + "}"
    \\  }
    \\
    \\  iteratorValue(iterator) {
    \\    return MapEntry.new(
    \\        keyIteratorValue_(iterator),
    \\        valueIteratorValue_(iterator))
    \\  }
    \\}
    \\
    \\class MapEntry {
    \\  construct new(key, value) {
    \\    _key = key
    \\    _value = value
    \\  }
    \\
    \\  key { _key }
    \\  value { _value }
    \\
    \\  toString { "%(_key):%(_value)" }
    \\}
    \\
    \\class MapKeySequence is Sequence {
    \\  construct new(map) {
    \\    _map = map
    \\  }
    \\
    \\  iterate(n) { _map.iterate(n) }
    \\  iteratorValue(iterator) { _map.keyIteratorValue_(iterator) }
    \\}
    \\
    \\class MapValueSequence is Sequence {
    \\  construct new(map) {
    \\    _map = map
    \\  }
    \\
    \\  iterate(n) { _map.iterate(n) }
    \\  iteratorValue(iterator) { _map.valueIteratorValue_(iterator) }
    \\}
    \\
    \\class Range is Sequence {}
    \\
    \\class System {
    \\  static print() {
    \\    writeString_("\n")
    \\  }
    \\
    \\  static print(obj) {
    \\    writeObject_(obj)
    \\    writeString_("\n")
    \\    return obj
    \\  }
    \\
    \\  static printAll(sequence) {
    \\    for (object in sequence) writeObject_(object)
    \\    writeString_("\n")
    \\  }
    \\
    \\  static write(obj) {
    \\    writeObject_(obj)
    \\    return obj
    \\  }
    \\
    \\  static writeAll(sequence) {
    \\    for (object in sequence) writeObject_(object)
    \\  }
    \\
    \\  static writeObject_(obj) {
    \\    var string = obj.toString
    \\    if (string is String) {
    \\      writeString_(string)
    \\    } else {
    \\      writeString_("[invalid toString]")
    \\    }
    \\  }
    \\}
    \\
    \\class ClassAttributes {
    \\  self { _attributes }
    \\  methods { _methods }
    \\  construct new(attributes, methods) {
    \\    _attributes = attributes
    \\    _methods = methods
    \\  }
    \\  toString { "attributes:%(_attributes) methods:%(_methods)" }
    \\}
    \\
;

pub const metaModuleSource: []const u8 =
    \\class Meta {
    \\  static getModuleVariables(module) {
    \\    if (!(module is String)) Fiber.abort("Module name must be a string.")
    \\    var result = getModuleVariables_(module)
    \\    if (result != null) return result
    \\
    \\    Fiber.abort("Could not find a module named '%(module)'.")
    \\  }
    \\
    \\  static eval(source) {
    \\    if (!(source is String)) Fiber.abort("Source code must be a string.")
    \\
    \\    var closure = compile_(source, false, false)
    \\    // TODO: Include compile errors.
    \\    if (closure == null) Fiber.abort("Could not compile source code.")
    \\
    \\    closure.call()
    \\  }
    \\
    \\  static compileExpression(source) {
    \\    if (!(source is String)) Fiber.abort("Source code must be a string.")
    \\    return compile_(source, true, true)
    \\  }
    \\
    \\  static compile(source) {
    \\    if (!(source is String)) Fiber.abort("Source code must be a string.")
    \\    return compile_(source, false, true)
    \\  }
    \\
    \\  foreign static compile_(source, isExpression, printErrors)
    \\  foreign static getModuleVariables_(module)
    \\}
    \\
;

pub const randomModuleSource: []const u8 =
    \\foreign class Random {
    \\  construct new() {
    \\    seed_()
    \\  }
    \\
    \\  construct new(seed) {
    \\    if (seed is Num) {
    \\      seed_(seed)
    \\    } else if (seed is Sequence) {
    \\      if (seed.isEmpty) Fiber.abort("Sequence cannot be empty.")
    \\
    \\      // TODO: Empty sequence.
    \\      var seeds = []
    \\      for (element in seed) {
    \\        if (!(element is Num)) Fiber.abort("Sequence elements must all be numbers.")
    \\
    \\        seeds.add(element)
    \\        if (seeds.count == 16) break
    \\      }
    \\
    \\      // Cycle the values to fill in any missing slots.
    \\      var i = 0
    \\      while (seeds.count < 16) {
    \\        seeds.add(seeds[i])
    \\        i = i + 1
    \\      }
    \\
    \\      seed_(
    \\          seeds[0], seeds[1], seeds[2], seeds[3],
    \\          seeds[4], seeds[5], seeds[6], seeds[7],
    \\          seeds[8], seeds[9], seeds[10], seeds[11],
    \\          seeds[12], seeds[13], seeds[14], seeds[15])
    \\    } else {
    \\      Fiber.abort("Seed must be a number or a sequence of numbers.")
    \\    }
    \\  }
    \\
    \\  foreign seed_()
    \\  foreign seed_(seed)
    \\  foreign seed_(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16)
    \\
    \\  foreign float()
    \\  float(end) { float() * end }
    \\  float(start, end) { float() * (end - start) + start }
    \\
    \\  foreign int()
    \\  int(end) { (float() * end).floor }
    \\  int(start, end) { (float() * (end - start)).floor + start }
    \\
    \\  sample(list) {
    \\    if (list.count == 0) Fiber.abort("Not enough elements to sample.")
    \\    return list[int(list.count)]
    \\  }
    \\  sample(list, count) {
    \\    if (count > list.count) Fiber.abort("Not enough elements to sample.")
    \\
    \\    var result = []
    \\
    \\    // The algorithm described in "Programming pearls: a sample of brilliance".
    \\    // Use a hash map for sample sizes less than 1/4 of the population size and
    \\    // an array of booleans for larger samples. This simple heuristic improves
    \\    // performance for large sample sizes as well as reduces memory usage.
    \\    if (count * 4 < list.count) {
    \\      var picked = {}
    \\      for (i in list.count - count...list.count) {
    \\        var index = int(i + 1)
    \\        if (picked.containsKey(index)) index = i
    \\        picked[index] = true
    \\        result.add(list[index])
    \\      }
    \\    } else {
    \\      var picked = List.filled(list.count, false)
    \\      for (i in list.count - count...list.count) {
    \\        var index = int(i + 1)
    \\        if (picked[index]) index = i
    \\        picked[index] = true
    \\        result.add(list[index])
    \\      }
    \\    }
    \\
    \\    return result
    \\  }
    \\
    \\  shuffle(list) {
    \\    if (list.isEmpty) return
    \\
    \\    // Fisher-Yates shuffle.
    \\    for (i in 0...list.count - 1) {
    \\      var from = int(i, list.count)
    \\      var temp = list[from]
    \\      list[from] = list[i]
    \\      list[i] = temp
    \\    }
    \\  }
    \\}
    \\
;
