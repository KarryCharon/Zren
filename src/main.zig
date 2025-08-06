const testing = @import("./testing/main.zig");

pub fn main() !u8 {
    return testing.testRun();
}
