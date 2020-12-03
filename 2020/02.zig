const std = @import("std");

fn readToken(it: *std.mem.TokenIterator) ![]const u8 {
    return it.next() orelse unreachable;
}

fn checkEasy(num1: u64, num2: u64, needle: u8, haystack: []const u8) bool {
    var count: u64 = 0;
    for (haystack) |ch| {
        if (ch == needle) {
            count += 1;
        }
    }

    return count >= num1 and count <= num2;
}

fn checkHard(num1: u64, num2: u64, needle: u8, haystack: []const u8) bool {
    return (haystack[num1 - 1] == needle) != (haystack[num2 - 1] == needle);
}

const Policy = fn (u64, u64, u8, []const u8) bool;

fn solve(input: []u8, policy: Policy) !u64 {
    var result: u64 = 0;
    var it = std.mem.tokenize(input, ":- \n");
    while (it.next()) |token| {
        const num1 = try std.fmt.parseInt(u64, token, 10);
        const num2 = try std.fmt.parseInt(u64, try readToken(&it), 10);
        const needle = (try readToken(&it))[0];
        const haystack = try readToken(&it);

        if (policy(num1, num2, needle, haystack)) {
            result += 1;
        }
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;
    defer std.debug.assert(!gpa.deinit());

    const input = try std.fs.cwd().readFileAlloc(allocator, "02.txt", 1024 * 1024);
    defer allocator.free(input);

    std.debug.print("EASY: {}\n", .{try solve(input, checkEasy)});
    std.debug.print("HARD: {}\n", .{try solve(input, checkHard)});
}
