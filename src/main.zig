const std = @import("std");
const tokenizer = @import("tokenizer");

pub fn main() !void {
    std.debug.print("Hi", .{});
    _ = tokenizer.Tokenizer;
}
