const std = @import("std");

pub const stdout = struct {
    pub inline fn print(comptime fmt: []const u8, args: anytype) void {
        std.io.getStdOut().writer().print(fmt, args) catch unreachable;
    }
};

pub const stderr = struct {
    pub inline fn print(comptime fmt: []const u8, args: anytype) void {
        std.io.getStdErr().writer().print(fmt, args) catch unreachable;
    }
};
