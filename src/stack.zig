const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
pub fn GenericStack(comptime T: type) type {
    return struct {
        allocator: Allocator,
        buffer: []T,
        top: usize = 0,

        pub fn init(allocator: Allocator) @This() {
            return .{
                .allocator = allocator,
                .buffer = &[_]T{},
            };
        }

        pub fn deinit(self: *@This()) void {
            self.allocator.free(self.buffer);
        }

        pub fn allocatedSize(self: *@This()) usize {
            return self.buffer.len * @sizeOf(T);
        }

        pub inline fn push(self: *@This(), item: T) void {
            self.buffer[self.top] = item;
            self.top += 1;
        }

        pub inline fn pop(self: *@This()) *T {
            self.top -= 1;
            return &self.buffer[self.top];
        }

        pub inline fn drop(self: *@This()) void {
            self.top -= 1;
        }

        pub inline fn write(self: *@This(), i: usize, item: *const T) void {
            self.buffer[i] = item.*;
        }

        pub inline fn clear(self: *@This()) void {
            self.top = 0;
        }

        pub inline fn stackTop(self: *@This()) *T {
            return &self.buffer[self.top - 1];
        }

        pub inline fn pOffset(self: *@This(), n: usize) void {
            self.top += n;
        }

        pub inline fn nOffset(self: *@This(), n: usize) void {
            self.top -= n;
        }

        pub inline fn topNegSlice(self: *@This(), n: usize) []T {
            return self.buffer[self.top - n .. self.top];
        }

        pub inline fn peek(self: *@This(), n: usize) *T {
            return &self.buffer[self.top - n];
        }

        pub inline fn peek1(self: *@This()) *T {
            return &self.buffer[self.top - 1];
        }

        pub inline fn peek2(self: *@This()) *T {
            return &self.buffer[self.top - 2];
        }

        pub inline fn isEmpty(self: *@This()) bool {
            return self.top == 0;
        }

        pub fn resize(self: *@This(), new_capacity: usize) !void {
            // 尝试remap
            if (self.allocator.remap(self.buffer, new_capacity)) |new_buf| {
                self.buffer = new_buf;
            } else {
                const new_buf = try self.allocator.alloc(T, new_capacity);
                @memcpy(new_buf[0..self.top], self.buffer[0..self.top]);
                self.allocator.free(self.buffer);
                self.buffer = new_buf;
            }
        }
    };
}

test "Stack" {
    const expect = std.testing.expect;

    var gpa = std.heap.DebugAllocator(.{}){};
    defer {
        // 尝试进行 deinit 操作
        const deinit_status = gpa.deinit();
        // 检测是否发生内存泄漏
        if (deinit_status == .leak) @panic("TEST FAIL");
    }
    const allocator = gpa.allocator();

    const IntStack = GenericStack(i32);

    var stack = IntStack.init(allocator);
    try stack.resize(4);
    defer stack.deinit();

    stack.push(1);
    stack.push(2);
    stack.push(3);

    try expect(stack.isEmpty() == false);

    try expect(stack.stackTop() == 3);
    try expect(stack.pop() == 3);
    try expect(stack.stackTop() == 2);
    try expect(stack.pop() == 2);
    try expect(stack.stackTop() == 1);
    try expect(stack.pop() == 1);

    try expect(stack.isEmpty() == true);
}
