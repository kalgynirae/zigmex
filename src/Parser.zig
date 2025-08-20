const std = @import("std");
const Allocator = std.mem.Allocator;
const Reader = std.io.Reader;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;

const Self = @This();

allocator: Allocator,
tokens: []const Token,
errors: std.ArrayListUnmanaged([]u8),
warnings: std.ArrayListUnmanaged([]u8),
tok_i: usize,

pub fn deinit(self: *Self, gpa: Allocator) void {
    self.errors.deinit(gpa);
    self.warnings.deinit(gpa);
}

fn consume(self: *Self, comptime tag: Token.Tag) !Token {
    comptime if (tag == .eof) @compileError("tag must not be .eof");
    const tok = self.tokens[self.tok_i];
    if (tok.tag == tag) {
        self.tok_i += 1;
        std.debug.print("consume(.{s})\n", .{@tagName(tok.tag)});
        return tok;
    } else {
        return error.UnexpectedToken;
    }
}

fn consumeAny(self: *Self, comptime tags: []const Token.Tag) !Token {
    comptime for (tags) |tag| {
        if (tag == .eof) @compileError("tag must not be .eof");
    };
    const tok = self.tokens[self.tok_i];
    inline for (tags) |tag| {
        if (tok.tag == tag) {
            self.tok_i += 1;
            std.debug.print("consume(.{s})\n", .{@tagName(tok.tag)});
            return tok;
        }
    }
    return error.UnexpectedToken;
}

fn nextIs(self: *const Self, comptime tag: Token.Tag) bool {
    return self.tokens[self.tok_i].tag == tag;
}

fn skip(self: *Self, comptime tags: []const Token.Tag) void {
    comptime for (tags) |tag| {
        if (tag == .eof) @compileError("tag must not be .eof");
    };
    loop: while (true) {
        const tok = self.tokens[self.tok_i];
        inline for (tags) |tag| {
            if (tok.tag == tag) {
                self.tok_i += 1;
                std.debug.print("skip(.{s})\n", .{@tagName(tok.tag)});
                continue :loop;
            }
        }
        break;
    }
}

fn parseAny(self: *Self) error{UnexpectedToken}!bool {
    std.debug.print("parseAny()\n", .{});
    errdefer std.debug.print("-> parseAny() false\n", .{});
    self.skip(&.{.blank_line});
    if (try self.parseBlock()) return true;
    if (try self.parseRaw()) return true;
    if (try self.parseSpan()) return true;
    std.debug.print("-> parseAny() true\n", .{});
    return error.UnexpectedToken;
}

fn parseBlock(self: *Self) !bool {
    std.debug.print("parseBlock()\n", .{});
    errdefer std.debug.print("-> parseBlock() false\n", .{});
    _ = self.consume(.coloncolon) catch return false;
    _ = try self.consume(.word);
    _ = try self.parseArgs();
    _ = try self.consume(.indent);
    while (true) {
        _ = self.parseAny() catch break;
    }
    _ = try self.consume(.dedent);
    std.debug.print("-> parseBlock() true\n", .{});
    return true;
}

fn parseArgs(self: *Self) !bool {
    std.debug.print("parseArgs()\n", .{});
    errdefer std.debug.print("-> parseArgs() false\n", .{});
    _ = self.consume(.l_paren) catch return false;
    while (true) : (self.skip(&.{ .comma, .space })) {
        _ = self.consume(.word) catch break;
        _ = self.consume(.equals) catch continue;
        _ = try self.consumeAny(&.{ .word, .text });
    }
    _ = try self.consume(.r_paren);
    std.debug.print("-> parseArgs() true\n", .{});
    return true;
}

fn parseRaw(self: *Self) !bool {
    std.debug.print("parseRaw()\n", .{});
    errdefer std.debug.print("-> parseRaw() false\n", .{});
    _ = self.consume(.bangbang) catch return false;
    _ = try self.consume(.word);
    _ = try self.parseArgs();
    _ = try self.consume(.indent);
    while (true) {
        _ = self.parseAny() catch break;
    }
    _ = try self.consume(.dedent);
    std.debug.print("-> parseRaw() true\n", .{});
    return true;
}

fn parseSpan(self: *Self) !bool {
    std.debug.print("parseSpan()\n", .{});
    errdefer std.debug.print("-> parseSpan() false\n", .{});
    const allowed = .{
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
        .star,
        .starstar,
        .bar,
        .barbar,
        .underscore,
        .underscoreunderscore,
        .backtick,
        .backtickbacktick,
        .l_paren,
        .r_paren,
        .l_brace,
        .r_brace,
    };
    _ = self.consumeAny(&allowed) catch return false;
    while (true) {
        _ = self.consumeAny(&allowed) catch break;
    }
    std.debug.print("-> parseSpan() true\n", .{});
    return true;
}

pub fn parse(gpa: Allocator, source: [:0]const u8, verbosity: Tokenizer.Verbosity) !Self {
    var tokenizer = try Tokenizer.init(gpa, source, verbosity);
    defer tokenizer.deinit(gpa);

    var tokens: std.ArrayListUnmanaged(Token) = .empty;
    errdefer tokens.deinit(gpa);
    while (true) {
        const token = tokenizer.next();
        if (token.tag == .eof) break;
        try tokens.append(gpa, token);
    }
    std.debug.assert(tokens.items.len > 0);

    var errors: std.ArrayListUnmanaged([]u8) = .empty;
    errdefer errors.deinit(gpa);
    var warnings: std.ArrayListUnmanaged([]u8) = .empty;
    errdefer warnings.deinit(gpa);

    const tokens_slice = try tokens.toOwnedSlice(gpa);
    errdefer gpa.free(tokens_slice);

    var parser: Self = .{
        .allocator = gpa,
        .tokens = tokens_slice,
        .errors = errors,
        .warnings = warnings,
        .tok_i = 0,
    };
    while (parser.tokens[parser.tok_i].tag != .eof) {
        _ = try parser.parseAny();
    }
    return parser;
}

pub fn parseFile(gpa: Allocator, path: []const u8, verbosity: Tokenizer.Verbosity) !Self {
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
    ;
    var parser = try parse(std.testing.allocator, input, .quiet);
    defer parser.deinit(std.testing.allocator);
}
