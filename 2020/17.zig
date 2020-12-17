const std = @import("std");

const Cube = struct {
    x: i64,
    y: i64,
    z: i64,
    w: i64,
};

const Map = std.AutoHashMap(Cube, void);

const IterationCount = 6;

const ProblemType = enum {
    easy,
    hard,
};

fn countNeighbors(problem_type: ProblemType, map: *const Map, cube: *const Cube, maybe_inactives: ?*Map) !u64 {
    var result: u64 = 0;
    const deltas = &[_]i64{ -1, 0, 1 };
    const dw_deltas = if (problem_type == .easy) &[_]i64{0} else deltas;
    for (deltas) |dx| {
        for (deltas) |dy| {
            for (deltas) |dz| {
                for (dw_deltas) |dw| {
                    if (dx == 0 and dy == 0 and dz == 0 and (problem_type == .easy or dw == 0)) {
                        continue;
                    }
                    const neighbor = Cube{
                        .x = cube.x + dx,
                        .y = cube.y + dy,
                        .z = cube.z + dz,
                        .w = cube.w + dw,
                    };
                    if (map.contains(neighbor)) {
                        result += 1;
                    } else if (maybe_inactives) |inactives| {
                        try inactives.put(neighbor, {});
                    }
                }
            }
        }
    }
    return result;
}

fn iterate(problem_type: ProblemType, from: *Map, to: *Map, inactives: *Map) !void {
    to.clearRetainingCapacity();
    inactives.clearRetainingCapacity();
    var from_it = from.iterator();
    while (from_it.next()) |kv| {
        const cube = &kv.key;
        const neighbor_count = try countNeighbors(problem_type, from, cube, inactives);
        if (neighbor_count == 2 or neighbor_count == 3) {
            try to.put(cube.*, {});
        }
    }
    var inactive_it = inactives.iterator();
    while (inactive_it.next()) |kv| {
        if ((try countNeighbors(problem_type, from, &kv.key, null)) == 3) {
            try to.put(kv.key, {});
        }
    }
}

fn solve(problem_type: ProblemType, original_map: *const Map, maps: []Map, inactives: *Map) !usize {
    for (maps) |*m| {
        m.clearRetainingCapacity();
    }
    var it = original_map.iterator();
    while (it.next()) |kv| {
        try maps[0].put(kv.key, {});
    }
    var idx: usize = 0;
    while (idx < IterationCount) : (idx += 1) {
        const from = &maps[idx % 2];
        const to = &maps[1 - idx % 2];
        try iterate(problem_type, from, to, inactives);
    }
    return maps[IterationCount % 2].count();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var input = try std.fs.cwd().openFile("17.txt", .{ .read = true });
    var reader = input.reader();

    var x: i64 = 0;
    var y: i64 = 0;

    var original_map = Map.init(allocator);
    var map1 = Map.init(allocator);
    var map2 = Map.init(allocator);
    var inactives = Map.init(allocator);
    var maps = [_]Map{ map2, map1 };

    defer {
        inactives.deinit();
        for (maps) |*m| {
            m.deinit();
        }
        original_map.deinit();
    }

    while (reader.readByte()) |ch| {
        if (ch == '\n') {
            y += 1;
            x = 0;
        } else {
            if (ch == '#') {
                try original_map.put(Cube{ .x = x, .y = y, .z = 0, .w = 0 }, {});
            }
            x += 1;
        }
    } else |err| {
        if (err != error.EndOfStream) return err;
    }

    std.debug.print("EASY: {}\n", .{solve(ProblemType.easy, &original_map, maps[0..], &inactives)});
    std.debug.print("HARD: {}\n", .{solve(ProblemType.hard, &original_map, maps[0..], &inactives)});
}
