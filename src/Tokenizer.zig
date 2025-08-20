const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayListUnmanaged;

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const Tag = enum {
        invalid,
        eof,
        blank_line,
        indent,
        dedent,
        word,
        space,
        text,
        newline,
        comma,
        equals,
        dot,
        dotdot,
        colon,
        coloncolon,
        bang,
        bangbang,
        star,
        starstar,
        bar,
        barbar,
        underscore,
        underscoreunderscore,
        backtick,
        backtickbacktick,
        l_paren,
        r_paren,
        l_brace,
        r_brace,
    };
};

const Self = @This();

buffer: [:0]const u8,
index: usize,
next_state: State,
indents: ArrayList(u8),
debug: bool,

pub const Verbosity = enum {
    quiet,
    debug,
};

pub fn init(allocator: Allocator, buffer: [:0]const u8, verbosity: Verbosity) !Self {
    var indents = try ArrayList(u8).initCapacity(allocator, 16);
    indents.appendAssumeCapacity(0);
    return .{
        .buffer = buffer,
        .index = 0,
        .next_state = .indentation,
        .indents = indents,
        .debug = verbosity == .debug,
    };
}

pub fn deinit(self: *Self, allocator: Allocator) void {
    self.indents.deinit(allocator);
}

pub fn advanceToEnd(self: *Self) !void {
    var token = self.next();
    for (1..10000) |_| {
        if (token.tag == .eof) return;
        token = self.next();
    }
    return error.IterationLimitReached;
}

const State = enum {
    indentation,
    default,
    word,
    whitespace,
    symbol,
    string,
    string_backslash,
    backslash,
    other,
};

pub fn next(self: *Self) Token {
    var result: Token = .{
        .tag = undefined,
        .loc = .{
            .start = self.index,
            .end = undefined,
        },
    };
    var end_override: ?usize = null;
    state: switch (self.next_state) {
        .indentation => {
            var temp_index = self.index;
            var found_newline = false;
            char: switch (self.buffer[temp_index]) {
                ' ', '\t' => {
                    temp_index += 1;
                    continue :char self.buffer[temp_index];
                },
                '\n' => {
                    temp_index += 1;
                    found_newline = true;
                },
                else => {},
            }
            const last_indent = self.indents.getLast();
            const new_indent: u8 = @truncate(temp_index - self.index);
            if (found_newline) {
                // Skip over the indent and yield a blank line
                self.index = temp_index;
                result.loc.start = self.index;
                result.tag = .blank_line;
            } else if (new_indent == last_indent) {
                // Skip over the indent without yielding a token
                self.index = temp_index;
                result.loc.start = temp_index;
                self.next_state = .default;
                continue :state .default;
            } else if (new_indent > last_indent) {
                if (self.debug) std.debug.print(
                    "indent: {} -> {}\n",
                    .{ last_indent, new_indent },
                );
                self.indents.appendAssumeCapacity(new_indent);
                self.index = temp_index;
                result.loc.start = self.index;
                end_override = self.index;
                result.tag = .indent;
                self.next_state = .default;
            } else {
                _ = self.indents.pop();
                const previous_indent = self.indents.getLast();
                if (self.debug) std.debug.print(
                    "dedent: {} -> {} (target: {})\n",
                    .{ last_indent, previous_indent, new_indent },
                );
                result.loc.start = temp_index;
                end_override = temp_index;
                result.tag = .dedent;
                if (previous_indent == new_indent) {
                    self.index = temp_index;
                    self.next_state = .default;
                }
            }
        },
        .default => switch (self.buffer[self.index]) {
            0 => {
                if (self.index == self.buffer.len) {
                    result.tag = .eof;
                } else {
                    self.index += 1;
                    result.tag = .invalid;
                }
            },
            '\n' => {
                self.index += 1;
                self.next_state = .indentation;
                result.tag = .newline;
            },
            ',' => {
                self.index += 1;
                result.tag = .comma;
            },
            '=' => {
                self.index += 1;
                result.tag = .equals;
            },
            '(' => {
                self.index += 1;
                result.tag = .l_paren;
            },
            ')' => {
                self.index += 1;
                result.tag = .r_paren;
            },
            '{' => {
                self.index += 1;
                result.tag = .l_brace;
            },
            '}' => {
                self.index += 1;
                result.tag = .r_brace;
            },
            'a'...'z', 'A'...'Z', '0'...'9', '-' => {
                continue :state .word;
            },
            ' ' => {
                continue :state .whitespace;
            },
            '.', ':', '!', '*', '|', '_', '`' => {
                continue :state .symbol;
            },
            '"' => {
                result.loc.start = self.index + 1;
                continue :state .string;
            },
            else => {
                continue :state .other;
            },
        },
        .word => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                'a'...'z', 'A'...'Z', '0'...'9', '-' => continue :state .word,
                else => {
                    result.tag = .word;
                },
            }
        },
        .whitespace => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                ' ' => continue :state .whitespace,
                else => {
                    result.tag = .space;
                },
            }
        },
        .symbol => {
            const first_symbol = self.buffer[self.index];
            self.index += 1;
            if (self.buffer[self.index] == first_symbol) {
                self.index += 1;
                result.tag = switch (first_symbol) {
                    '.' => .dotdot,
                    ':' => .coloncolon,
                    '!' => .bangbang,
                    '*' => .starstar,
                    '|' => .barbar,
                    '_' => .underscoreunderscore,
                    '`' => .backtickbacktick,
                    else => unreachable,
                };
            } else {
                result.tag = switch (first_symbol) {
                    '.' => .dot,
                    ':' => .colon,
                    '!' => .bang,
                    '*' => .star,
                    '|' => .bar,
                    '_' => .underscore,
                    '`' => .backtick,
                    else => unreachable,
                };
            }
        },
        .string => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                '"' => {
                    self.index += 1;
                    result.tag = .text;
                    end_override = self.index - 1;
                },
                '\\' => continue :state .string_backslash,
                else => continue :state .string,
            }
        },
        .string_backslash => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0, '\n' => result.tag = .invalid,
                else => continue :state .string,
            }
        },
        .backslash => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0, '\n' => {
                    result.tag = .invalid;
                },
                else => {
                    self.index += 1;
                    continue :state .default;
                },
            }
        },
        .other => {
            self.index += 1;
            switch (self.buffer[self.index]) {
                0, '\n', 'a'...'z', 'A'...'Z', '0'...'9', '-', ' ', ',', '=', '.', ':', '!', '*', '|', '_', '`', '(', ')', '{', '}' => {
                    result.tag = .text;
                },
                '\\' => continue :state .backslash,
                else => {
                    continue :state .other;
                },
            }
        },
    }

    result.loc.end = end_override orelse self.index;
    if (self.debug) std.debug.print(
        "TOKEN {s}(“{s}”)\n",
        .{ @tagName(result.tag), self.buffer[result.loc.start..result.loc.end] },
    );
    return result;
}

test "word" {
    const input = "..foo";
    var tokenizer = try init(std.testing.allocator, input, .quiet);
    defer tokenizer.deinit(std.testing.allocator);
    try std.testing.expectEqual(Token.Tag.dotdot, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.word, tokenizer.next().tag);
    try tokenizer.advanceToEnd();
}

test "params" {
    const input = "..foo(a, b=5, c=\"#|\\\":)\\\\str\")";
    var tokenizer = try init(std.testing.allocator, input, .quiet);
    defer tokenizer.deinit(std.testing.allocator);
    const tags = [_]Token.Tag{ .dotdot, .word, .l_paren, .word, .comma, .space, .word, .equals, .word, .comma, .space, .word, .equals, .text, .r_paren };
    for (tags) |tag| {
        try std.testing.expectEqual(tag, tokenizer.next().tag);
    }
    try tokenizer.advanceToEnd();
}

test "indentation" {
    const input =
        \\not indented
        \\  indented a bit
        \\    indented even more
        \\fully dedented
        \\    again indented
        \\  unexpectedly indented
        \\end
    ;
    var tokenizer = try init(std.testing.allocator, input, .quiet);
    defer tokenizer.deinit(std.testing.allocator);
    const indent_dedent_tags = [_]Token.Tag{ .indent, .indent, .dedent, .dedent, .indent, .dedent, .indent, .dedent };
    var i: usize = 0;
    var token = tokenizer.next();
    while (token.tag != .eof) : (token = tokenizer.next()) {
        switch (token.tag) {
            .indent, .dedent => {
                try std.testing.expectEqual(indent_dedent_tags[i], token.tag);
                i += 1;
            },
            else => {},
        }
    }
}

test "full example" {
    const input =
        \\..foo(test, cool-name="That's wild", type=15) Here's some
        \\stuff:*cool* :what:{bea|n|s}
        \\
        \\::test
        \\  this is indented
        \\
        \\  so is this
        \\
        \\this is not
        \\!what!{bea|n|s}
    ;
    var tokenizer = try init(std.testing.allocator, input, .quiet);
    defer tokenizer.deinit(std.testing.allocator);
    try tokenizer.advanceToEnd();
}
