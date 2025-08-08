const std = @import("std");
const builtin = @import("builtin");

pub fn strContains(str: []const u8, needle: []const u8) bool {
    return std.mem.indexOf(u8, str, needle) != null;
}

pub inline fn b8Tob16(b1: u8, b2: u8) u16 {
    return @as(u16, b1) << 8 | b2;
}

pub fn powerOf2Ceil(i: usize) usize {
    var n = i;
    n -= 1;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    return n + 1;
}

// 判断是否是局部变量(小写字母开头)
pub inline fn isLocalName(c: u8) bool {
    return c >= 'a' and c <= 'z';
}

pub inline fn isStaticFieldPrefix(c: u8) bool {
    return c == '_';
}

pub inline fn isName(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

pub inline fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

pub inline fn calcRangeIndex(start: usize, i: usize, step: isize) usize {
    return @intCast(@as(isize, @intCast(start)) + @as(isize, @intCast(i)) * step);
}

// 返回需要编码为UTF-8的[value]的字节数, 如果值太大, 无法编码, 则返回0
pub fn utf8EncodeNumBytes(value: u64) u8 {
    if (value <= 0x0000007f) return 1;
    if (value <= 0x000007ff) return 2;
    if (value <= 0x0000ffff) return 3;
    if (value <= 0x0010ffff) return 4;
    return 0;
}

// 将value编码为字节序列, 存储在[bytes]中, 假设[bytes]足够大. 返回编码的字节数
pub fn utf8Encode(value: u64, ptr: [*]u8) u8 {
    if (value <= 0x0000007f) {
        // 单字节(例如ASCII)
        ptr[0] = @intCast(value & 0x7f);
        return 1;
    }

    if (value <= 0x000007ff) {
        // 两字节序列: 110x_xxxx 10xx_xxxx
        ptr[0] = @intCast(0xc0 | ((value & 0x07c0) >> 6));
        ptr[1] = @intCast(0x80 | ((value & 0x003f) >> 0));
        return 2;
    }

    if (value <= 0x0000ffff) {
        // 三字节序列: 1110_xxxx 10xx_xxxx 10xx_xxxx
        ptr[0] = @intCast(0xe0 | ((value & 0xf000) >> 12));
        ptr[1] = @intCast(0x80 | ((value & 0x0fc0) >> 6));
        ptr[2] = @intCast(0x80 | ((value & 0x003f) >> 0));
        return 3;
    }

    if (value <= 0x0010ffff) {
        // 四字节序列: 1111_0xxx 10xx_xxxx 10xx_xxxx 10xx_xxxx
        ptr[0] = @intCast(0xf0 | ((value & 0x001c0000) >> 18));
        ptr[1] = @intCast(0x80 | ((value & 0x0003f000) >> 12));
        ptr[2] = @intCast(0x80 | ((value & 0x00000fc0) >> 6));
        ptr[3] = @intCast(0x80 | ((value & 0x0000003f) >> 0));
        return 4;
    }
    return 0;
}

// 返回以[byte]开头的UTF-8序列中的字节数, 如果字节不是UTF-8序列的开头, 返回 0 (ASCII)
pub fn utf8DecodeNumBytes(byte: u8) u8 {
    // 如果字节以10xxxxx开头, 则是UTF-8序列的中间部分, 不计算它
    if ((byte & 0xc0) == 0x80) return 0;

    // 第一个字节的高位代表UTF-8序列中的字节数
    if ((byte & 0xf8) == 0xf0) return 4;
    if ((byte & 0xf0) == 0xe0) return 3;
    if ((byte & 0xe0) == 0xc0) return 2;
    return 1;
}

// 从[bytes]开始解码UTF-8序列, 返回码点. 如果字节序列不是有效的UTF-8序列, 则返回null
pub fn utf8Decode(bytes: []const u8) ?u64 {
    // TODO 改进编码相关代码
    // TODO 改进返回类型 可能是?u32 或者?u21
    // 单字节 (i.e. fits in ASCII).
    var ptr = bytes.ptr;
    if (ptr[0] <= 0x7f) return ptr[0];
    var value: u64 = 0;
    var remaining_bytes: u32 = 0;
    if ((ptr[0] & 0xe0) == 0xc0) {
        // 双字节序: 110xxxxx 10xxxxxx.
        value = @intCast(ptr[0] & 0x1f);
        remaining_bytes = 1;
    } else if ((ptr[0] & 0xf0) == 0xe0) {
        // 三字节序: 1110xxxx 10xxxxxx 10xxxxxx.
        value = @intCast(ptr[0] & 0x0f);
        remaining_bytes = 2;
    } else if ((ptr[0] & 0xf8) == 0xf0) {
        // 四字节序: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx.
        value = @intCast(ptr[0] & 0x07);
        remaining_bytes = 3;
    } else {
        return null;
    }

    // 不读取超过缓冲区末尾的截断UTF-8.
    if (remaining_bytes > bytes.len - 1) return null;
    while (remaining_bytes > 0) {
        ptr += 1;
        remaining_bytes -= 1;
        // 剩余字节必须是10xxxxxx的形式
        if ((ptr[0] & 0xc0) != 0x80) return null;
        value = (value << 6) | (ptr[0] & 0x3f);
    }

    return value;
}

pub inline fn hashNum(num: anytype) u64 {
    return hashBits(@bitCast(num));
}

pub fn hashBits(bits: u64) u64 {
    var hash = bits;
    hash = ~hash +% (hash << 18);
    hash = hash ^ (hash >> 31);
    hash = hash *% 21;
    hash = hash ^ (hash >> 11);
    hash = hash +% (hash << 6);
    hash = hash ^ (hash >> 22);
    return hash & 0x3fff_ffff; // TODO 暂时截断(避免太大 影响取模性能)
}

pub inline fn hashStr(str: []const u8) u64 {
    return std.hash.Wyhash.hash(0, str) & 0x3fff_ffff; // TODO 可能需要改成u64
}

pub inline fn assert(cond: bool, comptime msg: []const u8) void {
    if (builtin.mode != .Debug) return;
    std.testing.expect(cond) catch @panic(msg);
}

pub fn bomSize(str: []const u8) u8 {
    if (std.mem.startsWith(u8, str, "\xFF\xFE")) return 2;
    if (std.mem.startsWith(u8, str, "\xFE\xFF")) return 2;
    if (std.mem.startsWith(u8, str, "\xEF\xBB\xBF")) return 3;
    if (std.mem.startsWith(u8, str, "\xFF\xFE\x00\x00")) return 4;
    if (std.mem.startsWith(u8, str, "\x00\x00\xFE\xFF")) return 4;
    return 0;
}

pub const fmt = struct {
    // 导入 C 标准库, 这里仅为了 对齐原实现
    const c = @cImport({
        @cInclude("stdio.h");
    });

    pub fn formatFloat14g(buffer: []u8, value: f64) ![]u8 {
        const len = c.snprintf(buffer.ptr, buffer.len, "%.14g", value);
        if (len < 0) return error.OutOfMemory else if (len >= buffer.len) return error.Overflow;
        return buffer[0..@intCast(len)];
    }
};

pub const Random = struct {
    prng: std.Random.DefaultPrng = undefined,
    random: std.Random = undefined,

    pub fn init(self: *Random) void {
        self.prng = std.Random.DefaultPrng.init(0);
        self.random = self.prng.random();
    }

    pub inline fn seed(self: *Random, s: u64) void {
        self.prng.seed(s);
    }

    pub inline fn int(self: *Random, t: type) t {
        return self.random.int(t);
    }

    pub inline fn float(self: *Random, t: type) t {
        return self.random.float(t);
    }
};
