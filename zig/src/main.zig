const std = @import("std");

pub fn main() !void {
    var c: [1]u8 = undefined;
    while(try std.io.getStdIn().read(&c) == 1 and c[0] != 'q') {
    }
}
