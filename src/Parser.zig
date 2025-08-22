const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Reader = std.io.Reader;
const StringHashMap = std.hash_map.StringHashMapUnmanaged;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

const Self = @This();

allocator: Allocator,
source: [:0]const u8,
tokens: []Token,
tok_i: usize,
debug: bool,
debug_depth: u8 = 0,

var debug_msg: [256]u8 = @splat(0);

pub const Verbosity = enum {
    quiet,
    debug,
};

pub fn deinit(self: *Self, gpa: Allocator) void {
    gpa.free(self.tokens);
}

fn log(self: Self, comptime fmt: []const u8, args: anytype) void {
    if (!self.debug) return;
    @memset(&debug_msg, 0);
    var i: u8 = 0;
    while (i < self.debug_depth * 2 and i < debug_msg.len) : (i += 1) {
        debug_msg[i] = if (i % 2 == 0) '|' else ' ';
    }
    const remaining_slice = debug_msg[i..debug_msg.len];
    _ = std.fmt.bufPrint(remaining_slice, fmt, args) catch |err| switch (err) {
        error.NoSpaceLeft => {
            debug_msg[debug_msg.len - 3] = '.';
            debug_msg[debug_msg.len - 2] = '.';
            debug_msg[debug_msg.len - 1] = '.';
        },
    };
    std.debug.print("{s}\n", .{debug_msg});
}

fn log_enter(self: *Self, comptime fmt: []const u8, args: anytype) void {
    self.log(fmt, args);
    self.debug_depth += 1;
}

fn log_exit(self: *Self, comptime fmt: []const u8, args: anytype) void {
    self.debug_depth -= 1;
    self.log("> " ++ fmt, args);
}

fn consume(self: *Self, tag: Token.Tag) !Token {
    std.debug.assert(tag != .eof);
    const tok = self.tokens[self.tok_i];
    if (tok.tag == tag) {
        self.tok_i += 1;
        self.log("consume(.{t})", .{tok.tag});
        return tok;
    } else {
        self.log("consume failed: wanted .{t} but got .{t}", .{ tag, tok.tag });
        return error.UnexpectedToken;
    }
}

fn consumeAny(self: *Self, comptime tags: []const Token.Tag) ?Token {
    comptime for (tags) |tag| {
        if (tag == .eof) @compileError("tag must not be .eof");
    };
    const tok = self.tokens[self.tok_i];
    inline for (tags) |tag| {
        if (tok.tag == tag) {
            self.tok_i += 1;
            self.log("consumeAny(.{t})", .{tok.tag});
            return tok;
        }
    }
    return null;
}

fn nextIsAny(self: *const Self, comptime tags: []const Token.Tag) bool {
    inline for (tags) |tag| {
        if (self.tokens[self.tok_i].tag == tag) return true;
    }
    return false;
}

fn skipAny(self: *Self, comptime tags: []const Token.Tag) void {
    comptime for (tags) |tag| {
        if (tag == .eof) @compileError("tag must not be .eof");
    };
    loop: while (true) {
        const tok = self.tokens[self.tok_i];
        inline for (tags) |tag| {
            if (tok.tag == tag) {
                self.tok_i += 1;
                self.log("skipAny(.{t})", .{tok.tag});
                continue :loop;
            }
        }
        break;
    }
}

const ParseError = error{
    OutOfMemory,
    UnexpectedToken,
};

fn parseAny(self: *Self, gpa: Allocator) ParseError!bool {
    self.log_enter("parseAny", .{});
    errdefer self.log_exit("err", .{});
    self.skipAny(&.{.blank_line});
    if (try self.parseBlock(gpa)) {
        self.log_exit("true", .{});
        return true;
    }
    if (try self.parseRaw(gpa)) {
        self.log_exit("true", .{});
        return true;
    }
    if (try self.parseSpan(gpa, null)) |s| {
        var span = s;
        defer span.deinit(gpa);
        self.log_exit("true", .{});
        return true;
    }
    self.log_exit("false", .{});
    return false;
}

fn parseBlock(self: *Self, gpa: Allocator) !bool {
    self.log_enter("parseBlock", .{});
    errdefer self.log_exit("false", .{});
    _ = self.consume(.coloncolon) catch {
        self.log_exit("false", .{});
        return false;
    };
    _ = try self.consume(.word);
    _ = try self.parseArgs(gpa);
    self.skipAny(&.{ .space, .newline });
    _ = try self.consume(.indent);
    while (try self.parseAny(gpa)) {}
    _ = try self.consume(.dedent);
    self.log_exit("true", .{});
    return true;
}

fn parseRaw(self: *Self, gpa: Allocator) !bool {
    self.log_enter("parseRaw", .{});
    errdefer self.log_exit("false", .{});
    _ = self.consume(.bangbang) catch {
        self.log_exit("false", .{});
        return false;
    };
    _ = try self.consume(.word);
    _ = try self.parseArgs(gpa);
    _ = try self.consume(.indent);
    while (try self.parseAny(gpa)) {}
    _ = try self.consume(.dedent);
    self.log_exit("true", .{});
    return true;
}

fn parseArgs(self: *Self, gpa: Allocator) !bool {
    self.log_enter("parseArgs", .{});
    _ = gpa;
    errdefer self.log_exit("false", .{});
    _ = self.consume(.l_paren) catch {
        self.log_exit("false", .{});
        return false;
    };
    while (true) : (self.skipAny(&.{ .comma, .space })) {
        _ = self.consume(.word) catch break;
        _ = self.consume(.equals) catch continue;
        _ = self.consumeAny(&.{ .word, .text }) orelse return error.UnexpectedToken;
    }
    _ = try self.consume(.r_paren);
    self.log_exit("true", .{});
    return true;
}

const Node = struct {
    name: []u8,
    args: ArrayList([]u8),
    kwargs: StringHashMap([]u8),
    data: NodeData,

    fn deinit(self: *@This(), gpa: Allocator) void {
        gpa.free(self.name);
        self.args.deinit(gpa);
        self.kwargs.deinit(gpa);
        self.data.deinit(gpa);
    }
};

const NodeData = union(enum) {
    text: []u8,
    nodes: []Node,

    fn deinit(self: *@This(), gpa: Allocator) void {
        switch (self.*) {
            .text => |text| gpa.free(text),
            .nodes => |nodes| {
                for (nodes) |n| {
                    var node = n;
                    node.deinit(gpa);
                }
                gpa.free(nodes);
            },
        }
    }
};

const text_tags = .{
    .word,
    .space,
    .text,
    .newline,
    .comma,
    .equals,
    .dot,
    .dotdot,
    .colon,
    .coloncolon,
    .bang,
    .bangbang,
    .l_paren,
    .r_paren,
};
const delimiter_tags = .{
    .star,
    .starstar,
    .bar,
    .barbar,
    .underscore,
    .underscoreunderscore,
    .backtick,
    .backtickbacktick,
    .l_brace,
    .r_brace,
};

fn parseSpan(self: *Self, gpa: Allocator, until_delimiter: ?Token.Tag) ParseError!?Node {
    self.log_enter("parseSpan", .{});
    var nodes: ArrayList(Node) = .empty;
    errdefer nodes.deinit(gpa);
    while (true) {
        if (until_delimiter) |d| if (self.tokens[self.tok_i].tag == d) break;
        const node = if (self.nextIsAny(&text_tags))
            (try self.parseTextSpan(gpa)).?
        else if (self.nextIsAny(&delimiter_tags))
            (try self.parseDelimiterSpan(gpa)).?
        else
            break;
        try nodes.append(gpa, node);
    }
    if (nodes.items.len == 0) {
        self.log_exit("none", .{});
        return null;
    }
    self.log_exit("nodes: ({})", .{nodes.items.len});
    return .{
        .name = try gpa.dupe(u8, "todo"),
        .args = .empty,
        .kwargs = .empty,
        .data = .{ .nodes = try nodes.toOwnedSlice(gpa) },
    };
}

pub fn parseTextSpan(self: *Self, gpa: Allocator) !?Node {
    std.debug.assert(self.nextIsAny(&text_tags));
    self.log_enter("parseTextSpan", .{});
    var text: ArrayList(u8) = .empty;
    errdefer text.deinit(gpa);
    while (self.nextIsAny(&text_tags)) {
        const token = self.consumeAny(&text_tags).?;
        switch (token.tag) {
            .newline => try text.append(gpa, ' '),
            else => try text.appendSlice(gpa, self.source[token.loc.start..token.loc.end]),
        }
    }
    const text_slice = try text.toOwnedSlice(gpa);
    self.log_exit("text: “{s}”", .{text_slice});
    return .{
        .name = try gpa.dupe(u8, "todo"),
        .args = .empty,
        .kwargs = .empty,
        .data = .{ .text = text_slice },
    };
}

pub fn parseDelimiterSpan(self: *Self, gpa: Allocator) !?Node {
    std.debug.assert(self.nextIsAny(&delimiter_tags));
    self.log_enter("parseDelimiterSpan", .{});
    const open_delimiter = self.consumeAny(&delimiter_tags).?.tag;
    const close_delimiter = switch (open_delimiter) {
        .l_brace => .r_brace,
        else => open_delimiter,
    };
    const inner_span = try self.parseSpan(gpa, close_delimiter);
    _ = try self.consume(close_delimiter);
    if (inner_span) |s| {
        const nodes = try gpa.alloc(Node, 1);
        nodes[0] = s;
        self.log_exit("inner span", .{});
        return .{
            .name = try gpa.dupe(u8, @tagName(open_delimiter)),
            .args = .empty,
            .kwargs = .empty,
            .data = .{ .nodes = nodes },
        };
    } else {
        self.log_exit("empty", .{});
        return .{
            .name = try gpa.dupe(u8, @tagName(open_delimiter)),
            .args = .empty,
            .kwargs = .empty,
            .data = .{ .text = "" },
        };
    }
}

pub fn parse(gpa: Allocator, source: [:0]const u8, verbosity: Verbosity) !Self {
    var tokenizer = try Tokenizer.init(gpa, source, .quiet);
    defer tokenizer.deinit(gpa);

    var tokens: ArrayList(Token) = .empty;
    errdefer tokens.deinit(gpa);
    while (true) {
        const token = tokenizer.next();
        try tokens.append(gpa, token);
        if (token.tag == .eof) break;
    }
    std.debug.assert(tokens.items.len > 0);

    const tokens_slice = try tokens.toOwnedSlice(gpa);
    errdefer gpa.free(tokens_slice);

    var parser: Self = .{
        .allocator = gpa,
        .source = source,
        .tokens = tokens_slice,
        .tok_i = 0,
        .debug = verbosity == .debug,
    };
    while (parser.tokens[parser.tok_i].tag != .eof) {
        _ = try parser.parseAny(gpa);
    }
    return parser;
}

pub fn parseFile(gpa: Allocator, path: []const u8, verbosity: Verbosity) !Self {
    const file = try std.fs.cwd().openFile(path, .{});
    const buffer = try file.readToEndAllocOptions(
        gpa,
        std.math.maxInt(usize),
        null,
        @alignOf(u8),
        0,
    );
    errdefer gpa.free(buffer);
    return try parse(gpa, buffer, verbosity);
}

test "parse" {
    const input =
        \\foo
        \\
        \\::bar(a, b=5, c, d="x y z")
        \\    test
        \\    test2 **test3** |foo *bar*|
        \\
    ;
    var parser = try parse(std.testing.allocator, input, .debug);
    defer parser.deinit(std.testing.allocator);
}
