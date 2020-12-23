const std = @import("std");

const Player = struct {
    nums: []u64,
    start_idx: usize,
    size: usize,

    fn init(allocator: *std.mem.Allocator, size_upper_bound: usize) !Player {
        return Player{
            .nums = try allocator.alloc(u64, size_upper_bound),
            .start_idx = 0,
            .size = 0,
        };
    }

    fn pop(self: *Player) u64 {
        const result = self.nums[self.start_idx];
        self.start_idx = (self.start_idx + 1) % self.nums.len;
        self.size -= 1;
        return result;
    }

    fn push(self: *Player, num: u64) void {
        self.nums[(self.start_idx + self.size) % self.nums.len] = num;
        self.size += 1;
    }

    fn isEmpty(self: *const Player) bool {
        return self.size == 0;
    }

    fn at(self: *const Player, pos: usize) u64 {
        return self.nums[(self.start_idx + pos) % self.nums.len];
    }

    fn score(self: *const Player) u64 {
        var result: u64 = 0;
        var pos: usize = 0;
        while (pos < self.size) : (pos += 1) {
            result += (self.size - pos) * self.at(pos);
        }
        return result;
    }

    fn asHashable(self: *const Player, allocator: *std.mem.Allocator) ![]u64 {
        var result = try allocator.alloc(u64, self.size);
        var pos: usize = 0;
        while (pos < self.size) : (pos += 1) {
            result[pos] = self.at(pos);
        }
        return result;
    }

    fn prefix(self: *Player, allocator: *std.mem.Allocator, n: u64) !Player {
        return Player{
            .nums = try allocator.dupe(u64, self.nums),
            .start_idx = self.start_idx,
            .size = n,
        };
    }
};

const Game = struct {
    player1: []u64,
    player2: []u64,

    fn init(allocator: *std.mem.Allocator, player1: *Player, player2: *Player) !Game {
        return Game{
            .player1 = try player1.asHashable(allocator),
            .player2 = try player2.asHashable(allocator),
        };
    }

    fn hash(self: Game) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&self.player1.len));
        std.hash.autoHashStrat(&hasher, self.player1, .Deep);
        hasher.update(std.mem.asBytes(&self.player2.len));
        std.hash.autoHashStrat(&hasher, self.player2, .Deep);
        return hasher.final();
    }

    fn eql(g1: Game, g2: Game) bool {
        return std.mem.eql(u64, g1.player1, g2.player1) and std.mem.eql(u64, g1.player2, g2.player2);
    }
};

const SeenGames = std.HashMap(Game, void, Game.hash, Game.eql, std.hash_map.DefaultMaxLoadPercentage);

const ProblemType = enum {
    easy,
    hard,
};

fn solveRec(allocator: *std.mem.Allocator, player1: *Player, player2: *Player, problem_type: ProblemType, level: u64) error{OutOfMemory}!void {
    var seen = SeenGames.init(allocator);
    while (!player1.isEmpty() and !player2.isEmpty()) {
        const game = try Game.init(allocator, player1, player2);
        if (seen.contains(game)) {
            player2.size = 0;
            return;
        }
        try seen.put(game, {});

        const p1 = player1.pop();
        const p2 = player2.pop();

        var player1_won = p1 > p2;

        if (problem_type == .hard and p1 <= player1.size and p2 <= player2.size) {
            var sub_player1 = try player1.prefix(allocator, p1);
            var sub_player2 = try player2.prefix(allocator, p2);
            try solveRec(allocator, &sub_player1, &sub_player2, problem_type, level + 1);
            player1_won = sub_player2.isEmpty();
        }

        if (player1_won) {
            player1.push(p1);
            player1.push(p2);
        } else {
            player2.push(p2);
            player2.push(p1);
        }
    }
}

fn solve(allocator: *std.mem.Allocator, input: []const u8, problem_type: ProblemType) !u64 {
    var size_upper_bound = std.mem.count(u8, input, "\n");
    var player1 = try Player.init(allocator, size_upper_bound);
    var player2 = try Player.init(allocator, size_upper_bound);

    var line_it = std.mem.split(input, "\n");
    var current_player = &player1;
    while (line_it.next()) |line| {
        if (line.len == 0) {
            current_player = &player2;
        } else {
            if (std.mem.indexOfScalar(u8, line, ':') == null) {
                current_player.push(try std.fmt.parseInt(u64, line, 10));
            }
        }
    }

    try solveRec(allocator, &player1, &player2, problem_type, 0);
    return if (player1.isEmpty()) player2.score() else player1.score();
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = &arena.allocator;

    const input = try std.fs.cwd().readFileAlloc(allocator, "22.txt", std.math.maxInt(usize));

    std.debug.print("EASY: {}\n", .{try solve(allocator, input, .easy)});
    std.debug.print("HARD: {}\n", .{try solve(allocator, input, .hard)});
}
