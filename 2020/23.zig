const std = @import("std");

const ProblemType = enum {
    easy,
    hard,
};

fn getDest(current: u64, n: u64) u64 {
    return if (current == 1) n else current - 1;
}

fn play_game(allocator: *std.mem.Allocator, prefix: []const u64, n: usize, moves: u64, problem_type: ProblemType) !u64 {
    var next = try allocator.alloc(u64, n + 1);
    for (prefix) |p, idx| {
        var np = &next[p];
        if (idx + 1 < prefix.len) {
            np.* = prefix[idx + 1];
        } else {
            np.* = if (prefix.len == n) prefix[0] else prefix.len + 1;
        }
    }
    if (n > prefix.len) {
        var idx: usize = prefix.len + 1;
        while (idx < n) : (idx += 1) {
            next[idx] = idx + 1;
        }
        next[n] = prefix[0];
    }

    var idx: usize = 0;
    var current: u64 = prefix[0];

    while (idx < moves) : (idx += 1) {
        const a = next[current];
        const b = next[a];
        const c = next[b];

        var dest = getDest(current, n);
        while (dest == a or dest == b or dest == c) {
            dest = getDest(dest, n);
        }

        next[current] = next[c];
        next[c] = next[dest];
        next[dest] = a;

        current = next[current];
    }

    switch (problem_type) {
        .easy => {
            var result: u64 = 0;
            idx = 1;
            current = next[1];
            while (idx < n) : (idx += 1) {
                result = result * 10 + current;
                current = next[current];
            }
            return result;
        },
        .hard => {
            const star1 = next[1];
            const star2 = next[star1];
            return star1 * star2;
        },
    }
}

pub fn main() !void {
    const input = @embedFile("23.txt");
    var prefix = [_]u64{0} ** input.len;
    for (input) |ch, idx| {
        prefix[idx] = ch - '0';
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const prefixSlice = prefix[0..];
    std.debug.print("EASY: {}\n", .{try play_game(&arena.allocator, prefixSlice, prefix.len, 100, .easy)});
    std.debug.print("HARD: {}\n", .{try play_game(&arena.allocator, prefixSlice, 1_000_000, 10_000_000, .hard)});
}
