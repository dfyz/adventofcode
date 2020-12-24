const std = @import("std");

fn moveX(ch: u8, x: *i64, base: i64) void {
    switch (ch) {
        'e' => x.* += base,
        'w' => x.* -= base,
        else => unreachable,
    }
}

fn moveY(ch: u8, y: *i64) void {
    switch (ch) {
        'n' => y.* += 2,
        's' => y.* -= 2,
        else => unreachable,
    }
}

fn move(x: *i64, y: *i64, line: []const u8) void {
    var idx: usize = 0;
    while (idx < line.len) : (idx += 1) {
        const ch = line[idx];
        switch (line[idx]) {
            'w', 'e' => moveX(ch, x, 2),
            else => {
                moveY(ch, y);
                moveX(line[idx + 1], x, 1);
                idx += 1;
            },
        }
    }
}

const Pos = struct {
    x: i64,
    y: i64,

    fn hash(self: Pos) u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(std.mem.asBytes(&self.x));
        h.update(std.mem.asBytes(&self.y));
        return h.final();
    }

    fn eql(a: Pos, b: Pos) bool {
        return a.x == b.x and a.y == b.y;
    }

    fn countNeighbors(self: *const Pos, black_tiles: *const Positions, white_tiles_out: ?*Positions) !u64 {
        const deltas = [_]Pos{
            .{ .x = -2, .y = 0 },
            .{ .x = 2, .y = 0 },
            .{ .x = 1, .y = 2 },
            .{ .x = 1, .y = -2 },
            .{ .x = -1, .y = 2 },
            .{ .x = -1, .y = -2 },
        };
        var result: u64 = 0;

        for (deltas) |*d| {
            const pos = Pos{ .x = self.x + d.x, .y = self.y + d.y };
            const is_black = black_tiles.contains(pos);
            if (is_black) {
                result += 1;
            } else if (white_tiles_out) |whites| {
                try whites.put(pos, {});
            }
        }

        return result;
    }
};

const Positions = std.HashMap(Pos, void, Pos.hash, Pos.eql, std.hash_map.DefaultMaxLoadPercentage);

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var allocator = &arena.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "24.txt", std.math.maxInt(usize));
    var black_tiles_1 = Positions.init(allocator);
    var it = std.mem.tokenize(input, "\n");
    while (it.next()) |line| {
        var x: i64 = 0;
        var y: i64 = 0;
        move(&x, &y, line);
        const pos = Pos{ .x = x, .y = y };
        if (black_tiles_1.contains(pos)) {
            black_tiles_1.removeAssertDiscard(pos);
        } else {
            try black_tiles_1.put(pos, {});
        }
    }

    std.debug.print("EASY: {}\n", .{black_tiles_1.count()});

    var black_tiles_2 = Positions.init(allocator);
    var black_tiles = [_]*Positions{ &black_tiles_1, &black_tiles_2 };
    var white_tiles = Positions.init(allocator);

    var idx: usize = 0;
    var day: usize = 0;
    while (day < 100) : ({
        day += 1;
        idx = 1 - idx;
    }) {
        var from = black_tiles[idx];
        var to = black_tiles[1 - idx];

        to.clearRetainingCapacity();
        white_tiles.clearRetainingCapacity();

        var black_it = from.iterator();
        while (black_it.next()) |black| {
            const neighbor_count = try black.key.countNeighbors(from, &white_tiles);
            if (neighbor_count == 1 or neighbor_count == 2) {
                try to.put(black.key, {});
            }
        }

        var white_it = white_tiles.iterator();
        while (white_it.next()) |white| {
            if ((try white.key.countNeighbors(from, null)) == 2) {
                try to.put(white.key, {});
            }
        }
    }
    std.debug.print("HARD: {}\n", .{black_tiles[idx].count()});
}
