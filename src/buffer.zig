const std = @import("std");
const Utils = @import("utils.zig");

pub fn GenericBuffer(comptime Element: type) type {
    return struct {
        const T = Element;
        allocator: std.mem.Allocator,
        data: std.ArrayList(T),
        count: usize = 0,

        pub fn init(allocator: std.mem.Allocator) GenericBuffer(T) {
            return .{
                .allocator = allocator,
                .data = std.ArrayList(T).init(allocator),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.data.deinit();
        }

        pub inline fn indexByRef(self: *@This(), v: *T) ?usize {
            return std.mem.indexOfScalar(T, self.data.items, v.*);
        }

        pub inline fn index(self: *@This(), v: T) ?usize {
            return std.mem.indexOfScalar(T, self.data.items, v);
        }

        pub inline fn len(self: *@This()) usize {
            return self.data.items.len;
        }

        pub inline fn at(self: *@This(), i: usize) T {
            return self.data.items[i];
        }

        pub inline fn rat(self: *const @This(), i: usize) *T {
            return &self.data.items.ptr[i]; // TODO 这里应当使用安全访问
        }

        pub fn clear(self: *@This()) void {
            self.data.clearAndFree();
            self.count = 0;
        }

        pub fn fill(self: *@This(), data: T, count: usize) void {
            self.data.ensureTotalCapacity(self.count + count) catch unreachable;
            self.data.items.len = self.data.capacity;
            for (0..count) |_| {
                self.data.items[self.count] = data;
                self.count += 1;
            }
        }

        pub fn write(self: *@This(), data: T) usize {
            self.fill(data, 1);
            return self.count - 1;
        }

        pub inline fn push(self: *@This(), data: T) void {
            self.fill(data, 1);
        }

        pub inline fn reserve(self: *@This(), count: usize) void {
            self.data.ensureTotalCapacity(count) catch unreachable;
        }

        pub inline fn resize(self: *@This(), count: usize) void {
            self.data.ensureTotalCapacity(self.count + count) catch unreachable;
            self.data.resize(count) catch unreachable;
            self.count = count;
        }

        pub inline fn allocatedSize(self: *const @This()) usize {
            return self.data.capacity * @sizeOf(T);
        }

        const IterWrap = struct {
            data: []T,
            from: usize,
            pos: usize,
            pub fn rbyte(self: *@This()) u8 {
                self.pos += 1;
                return self.data[self.pos - 1];
            }

            pub fn rshort(self: *@This()) u16 {
                self.pos += 2;
                return Utils.b8Tob16(self.data[self.pos - 2], self.data[self.pos - 1]);
            }

            pub fn peek(self: *@This(), i: usize) u8 {
                return self.data[self.pos + i];
            }
        };

        pub fn iterWrap(self: *@This(), from: usize) IterWrap {
            return .{
                .data = self.data.items,
                .from = from,
                .pos = from,
            };
        }
    };
}
