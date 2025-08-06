const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const os = std.os;
const File = std.fs.File;

pub const PathType = enum {
    PATH_TYPE_ABSOLUTE, // 绝对路径: POSIX系统下以"/"开头，Windows系统下以盘符开头
    PATH_TYPE_RELATIVE, // 相对路径: 以"./"或"../"开头
    PATH_TYPE_SIMPLE, //   A path that has no leading prefix, like "foo/bar".
};

pub const Path = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    buffer: []u8 = &.{},

    pub fn init(allocator: std.mem.Allocator, path: []const u8) !Self {
        return .{ .allocator = allocator, .buffer = try allocator.dupe(u8, path) };
    }

    pub fn deinit(self: *Self) void {
        self.free(self.buffer);
    }

    inline fn free(self: *Self, path: []const u8) void {
        if (path.len > 0) self.allocator.free(path);
    }

    pub fn clear(self: *Self) void {
        self.deinit();
        self.buffer = &.{};
    }

    fn set(self: *@This(), chars: []u8) void {
        self.deinit();
        self.buffer = chars;
        _ = std.mem.replace(u8, self.buffer, "\\", "/", self.buffer);
    }

    pub fn push(self: *@This(), chars: []const u8) void {
        const path = std.mem.concat(self.allocator, u8, &.{ self.buffer, chars }) catch unreachable;
        self.set(path);
    }

    pub fn join(self: *Self, path: []const u8) Self {
        const p = std.fs.path.join(self.allocator, &.{ self.buffer, path }) catch unreachable;
        return .{ .allocator = self.allocator, .buffer = p };
    }

    pub fn joinInner(self: *@This(), chars: []const u8) void {
        const path = std.fs.path.join(self.allocator, &.{ self.buffer, chars }) catch unreachable;
        self.set(path);
    }

    pub fn removeExtension(self: *@This()) void {
        const ext = std.fs.path.extension(self.buffer);
        if (ext.len > 0) {
            self.buffer = self.buffer[0 .. self.buffer.len - ext.len];
        }
    }

    // 获取父目录
    pub fn parent(self: *const Self) !Self {
        if (self.buffer.len == 0) return error.NoParent;
        const dir = std.fs.path.dirname(self.buffer) orelse return error.NoParent;
        return Self.init(self.allocator, dir);
    }

    // 解析路径（规范化+绝对路径）
    pub fn resolveAbs(self: *const Self) !Self {
        var abs_path: []u8 = undefined;
        abs_path = normalizeAbs(self.allocator, self.buffer) catch unreachable;
        return .{ .allocator = self.allocator, .buffer = abs_path };
    }

    pub fn resolveAbsInner(self: *@This()) !void {
        var abs_path: []u8 = undefined;
        abs_path = normalizeAbs(self.allocator, self.buffer) catch unreachable;
        self.set(abs_path);
    }

    // 解析路径（规范化+相对路径）
    pub fn resolveRel(self: *const Self) !Self {
        var rel_path: []u8 = undefined;
        rel_path = normalizeRelative(self.allocator, self.buffer) catch unreachable;
        return .{ .allocator = self.allocator, .buffer = rel_path };
    }

    pub fn resolveRelInner(self: *@This()) !void {
        var rel_path: []u8 = undefined;
        rel_path = normalizeRelative(self.allocator, self.buffer) catch unreachable;
        self.set(rel_path);
    }

    pub fn normalizeAbs(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
        var buf = std.ArrayList([]const u8).init(allocator);
        buf.ensureTotalCapacity(8) catch unreachable;
        defer buf.deinit();
        const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
        defer allocator.free(cwd);
        if (path.len == 0 or path[0] == '.') buf.append(cwd) catch {};
        if (path.len > 0 and (path[0] == '/' or path[0] == '\\')) buf.append("/") catch {}; // posix系统下绝对路径
        var splitIter = std.mem.splitAny(u8, path, &.{ '\\', '/' });
        while (splitIter.next()) |part| {
            const p = part;
            if (p.len == 0 or std.mem.eql(u8, p, ".")) {
                continue;
            } else if (std.mem.eql(u8, p, "..")) {
                _ = buf.pop() orelse continue;
            } else if (std.mem.eql(u8, p, "...")) {
                _ = buf.pop() orelse continue;
                _ = buf.pop() orelse continue;
            } else {
                buf.append(p) catch unreachable;
            }
        }
        // 组合路径
        return std.fs.path.join(allocator, buf.items);
    }

    // 展开后仍是相对路径, 只保留一级相对路径(最前面)
    pub fn normalizeRelative(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
        var buf = std.ArrayList([]const u8).init(allocator);
        buf.ensureTotalCapacity(8) catch unreachable;
        defer buf.deinit();
        var splitIter = std.mem.splitAny(u8, path, &.{ '\\', '/' });
        while (splitIter.next()) |part| {
            const p = part;
            if (buf.items.len != 0 and std.mem.eql(u8, p, ".")) {
                continue;
            } else if (std.mem.eql(u8, p, "..")) {
                _ = buf.pop() orelse continue;
            } else if (std.mem.eql(u8, p, "...")) {
                _ = buf.pop() orelse continue;
                _ = buf.pop() orelse continue;
            } else {
                buf.append(p) catch unreachable;
            }
        }
        // 组合路径
        return std.fs.path.join(allocator, buf.items);
    }

    // 文件名（含扩展名）
    pub fn filename(self: *const Path) []const u8 {
        return std.fs.path.basename(self.buffer);
    }

    // 后缀名（带点）
    pub fn suffix(self: *const Path) ?[]const u8 {
        const ext = std.fs.path.extension(self.buffer);
        return if (ext.len > 0) ext else null;
    }

    // 检查路径是否存在
    pub fn exists(self: *const Path) bool {
        if (self.isAbsolute()) {
            std.fs.accessAbsolute(self.buffer, .{}) catch return false;
        } else {
            std.fs.cwd().access(self.buffer, .{}) catch return false;
        }
        return true;
    }

    // 获取绝对路径
    pub fn absolute(self: *const Self) !Self {
        return self.resolveAbs();
    }

    // 创建目录
    pub fn mkdir(self: *const Self, recursive: bool) !void {
        if (recursive) {
            try std.fs.cwd().makePath(self.buffer);
        } else {
            try std.fs.cwd().makeDir(self.buffer);
        }
    }

    // 创建文件
    pub fn mkfile(self: *const Self) !void {
        const f = try std.fs.cwd().createFile(self.buffer, .{});
        defer f.close();
    }

    // 文件名别名
    pub fn name(self: *const Self) []const u8 {
        return self.filename();
    }

    // 打开文件
    pub fn open(self: *const Self, flags: File.OpenFlags) File.OpenError!File {
        return std.fs.cwd().openFile(self.buffer, flags);
    }

    // 获取路径各部分
    pub fn parts(self: *const Self) ![][]const u8 {
        var iterator = std.fs.path.componentIterator(self.buffer) catch return error.InvalidPath;
        var components = std.ArrayList([]const u8).init(self.allocator);
        errdefer components.deinit();
        while (iterator.next()) |c| try components.append(c.name);
        return components.toOwnedSlice();
    }

    // 获取文件状态
    pub fn stat(self: *const Self) !fs.File.Stat {
        return std.fs.cwd().statFile(self.buffer);
    }

    // 读取整个文件
    pub fn readBytes(self: *const Self, allocator: std.mem.Allocator) ![]u8 {
        const file = try self.open(.{});
        defer file.close();
        const s = try file.stat();
        return try file.readToEndAlloc(allocator, s.size);
    }

    // 写入整个文件
    pub fn writeBytes(self: *const Self, content: []const u8) !void {
        const file = try self.open(.{ .mode = .write_only });
        defer file.close();
        try file.writeAll(content);
    }

    // 删除目录
    pub fn rmdir(self: *const Self) !void {
        try fs.cwd().deleteDir(self.buffer);
    }

    // 文件名（不含扩展名）
    pub fn stem(self: *const Self) []const u8 {
        const base = self.filename();
        if (std.mem.lastIndexOfScalar(u8, base, '.')) |dot_index| {
            return base[0..dot_index];
        }
        return base;
    }

    // 替换文件名
    pub fn withName(self: *const Self, wname: []const u8) !Self {
        if (std.fs.path.dirname(self.buffer)) |dir| {
            return .{
                .allocator = self.allocator,
                .buffer = try std.fs.path.join(self.allocator, &.{ dir, wname }),
            };
        }
        return Self.init(self.allocator, wname);
    }

    // 替换后缀名
    pub fn withSuffix(self: *const Self, wsuffix: []const u8) !Self {
        const s = self.stem();
        var new_name: []const u8 = &.{};

        if (wsuffix.len > 0 and wsuffix[0] != '.') {
            new_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ s, wsuffix });
        } else {
            new_name = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ s, wsuffix });
        }
        defer self.allocator.free(new_name);

        return self.withName(new_name);
    }

    // 替换文件名（不含后缀）
    pub fn withStem(self: *const Path, new_stem: []const u8) !Path {
        if (self.suffix()) |s| {
            const n = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ new_stem, s });
            defer self.allocator.free(n);
            return self.withName(n);
        }
        return self.withName(new_stem);
    }

    // 检查是否是绝对路径
    pub inline fn isAbsolute(self: *const Path) bool {
        return std.fs.path.isAbsolute(self.buffer);
    }

    inline fn isRelativeEx(_: *@This(), chars: []const u8) bool {
        return chars[0] == '.' and std.fs.path.isSep(chars[1]);
    }

    pub inline fn isRelative(self: *@This()) bool {
        return self.isRelativeEx(self.buffer) or (self.buffer[0] == '.' and self.isRelativeEx(self.buffer[1..]));
    }

    // 检查是否是目录
    pub fn isDir(self: *const Path) bool {
        const s = self.stat() catch return false;
        return s.kind == .directory;
    }

    // 检查是否是文件
    pub fn isFile(self: *const Path) bool {
        const s = self.stat() catch return false;
        return s.kind == .file;
    }

    pub fn pathType(self: *@This()) PathType {
        if (self.isAbsolute()) return .PATH_TYPE_ABSOLUTE;
        if (self.isRelative()) return .PATH_TYPE_RELATIVE;
        return .PATH_TYPE_SIMPLE;
    }

    pub fn toString(self: *const Path) []const u8 {
        return self.buffer;
    }
};

// 测试用例
test "Path operations" {
    const allocator = std.heap.smp_allocator;
    const cwd = std.fs.cwd().realpathAlloc(allocator, ".") catch unreachable;
    defer allocator.free(cwd);
    std.debug.print("cwd: {s}\n", .{cwd});
    const tmp_dir = "./test_tmp_dir";

    // 准备测试环境
    std.fs.cwd().makeDir(tmp_dir) catch {};
    defer std.fs.cwd().deleteTree(tmp_dir) catch {};

    // 测试基本路径操作
    var p = try Path.init(allocator, tmp_dir);
    defer p.deinit();

    // 测试 join
    var p2 = p.join("file.txt");
    p2.mkfile() catch unreachable;
    defer p2.deinit();
    const p2join = std.fs.path.join(allocator, &.{ tmp_dir, "file.txt" }) catch unreachable;
    defer allocator.free(p2join);
    try std.testing.expectEqualStrings(p2join, p2.toString());

    // 测试文件操作
    std.debug.print("rel: {s}\n", .{p2.toString()});
    var absp2 = try p2.absolute();
    defer absp2.deinit();
    std.debug.print("abs: {s}\n", .{absp2.toString()});
    try absp2.writeBytes("Hello, Zig!");
    const content = try absp2.readBytes(p2.allocator);
    defer allocator.free(content);
    try std.testing.expectEqualStrings("Hello, Zig!", content);

    // 测试路径解析
    var abs = try p2.absolute();
    defer abs.deinit();
    try std.testing.expect(abs.isAbsolute());

    // 测试元数据操作
    try std.testing.expect(p2.exists());
    try std.testing.expect(p2.isFile());

    // 测试文件名操作
    try std.testing.expectEqualStrings("file.txt", p2.filename());
    try std.testing.expectEqualStrings("file", p2.stem());
    try std.testing.expectEqualStrings(".txt", p2.suffix().?);

    // 测试修改文件名
    var p3 = try p2.withName("newfile.zig");
    defer p3.deinit();
    try std.testing.expectEqualStrings("newfile.zig", p3.filename());

    // 测试修改后缀
    var p4 = try p3.withSuffix(".txt");
    defer p4.deinit();
    try std.testing.expectEqualStrings("newfile.txt", p4.filename());

    // 测试修改词干
    var p5 = try p4.withStem("final");
    defer p5.deinit();
    try std.testing.expectEqualStrings("final.txt", p5.filename());

    // 测试目录操作
    var p6 = p.join("subdir");
    defer p6.deinit();
    try p6.mkdir(true);
    try std.testing.expect(p6.isDir());
    try p6.rmdir();

    // 测试路径拆分
    std.debug.print("split: {s}\n", .{p6.toString()});
    const parts = try p6.parts();
    defer allocator.free(parts);
    try std.testing.expect(parts.len == 3);
    var true_parts = std.mem.splitScalar(u8, p6.buffer, std.fs.path.sep);
    for (0..parts.len) |i| {
        try std.testing.expectEqualStrings(true_parts.next().?, parts[i]);
    }

    // 测试joinInner
    var p7 = Path.init(allocator, "a") catch unreachable;
    defer p7.deinit();
    p7.joinInner("b");
    const true_joints = std.fs.path.join(allocator, &.{ "a", "b" }) catch unreachable;
    defer allocator.free(true_joints);
    try std.testing.expectEqualStrings(true_joints, p7.toString());

    // 测试 removeExtension
    var p8 = Path.init(allocator, "a.b.c") catch unreachable;
    defer p8.deinit();
    p8.removeExtension();
    try std.testing.expectEqualStrings("a.b", p8.toString());

    // 测试相对路径resolve
    var p9 = Path.init(allocator, "./a/b/c/d/e/../../../1.txt") catch unreachable;
    defer p9.deinit();
    p9.resolveRelInner() catch unreachable;
    try std.testing.expectEqualStrings("./a/b/1.txt", p9.toString());

    var p10 = Path.init(allocator, "./a/b/c/d/e/../../.././1.txt") catch unreachable;
    defer p10.deinit();
    p10.resolveRelInner() catch unreachable;
    try std.testing.expectEqualStrings("./a/b/1.txt", p10.toString());

    var p11 = Path.init(allocator, "a/b/c/d/e/../../.././1.txt") catch unreachable;
    defer p11.deinit();
    p11.resolveRelInner() catch unreachable;
    try std.testing.expectEqualStrings("a/b/1.txt", p11.toString());
}
