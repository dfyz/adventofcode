const std = @import("std");

const Field = struct {
    seats: []u8,
    rows: usize,
    cols: usize,
    stride: usize,

    fn getSeat(self: *Field, row: usize, col: usize, dr: i64, dc: i64) ?u8 {
        const next_row = @intCast(i64, row) + dr;
        const next_col = @intCast(i64, col) + dc;
        if (next_row < 0 or next_col < 0 or next_row >= self.rows or next_col >= self.cols) return null;
        const idx = @intCast(usize, next_row) * self.stride + @intCast(usize, next_col);
        return self.seats[idx];
    }
};

const IsOccupied = fn (field: *Field, row: usize, col: usize, stride: usize, dr: i64, dc: i64) bool;

fn isOccupiedEasy(field: *Field, row: usize, col: usize, stride: usize, dr: i64, dc: i64) bool {
    if (field.getSeat(row, col, dr, dc)) |adj_seat| {
        return adj_seat == '#';
    }
    return false;
}

fn isOccupiedHard(field: *Field, row: usize, col: usize, stride: usize, dr: i64, dc: i64) bool {
    var mul: i64 = 1;
    while (true) : (mul += 1) {
        switch (field.getSeat(row, col, dr * mul, dc * mul) orelse 'L') {
            '#' => return true,
            'L' => return false,
            else => {},
        }
    }
    unreachable;
}

fn evolve(allocator: *std.mem.Allocator, input: []u8, tolerance: u64, is_occupied: IsOccupied) !u64 {
    const cols = std.mem.indexOfScalar(u8, input, '\n').?;
    const stride = cols + 1;
    const rows = input.len / stride;

    var field1 = Field{
        .seats = try allocator.dupe(u8, input),
        .rows = rows,
        .cols = cols,
        .stride = stride,
    };

    var field2 = Field{
        .seats = try allocator.alloc(u8, field1.seats.len),
        .rows = rows,
        .cols = cols,
        .stride = stride,
    };
    std.mem.set(u8, field2.seats, '\n');

    var fields = [_]Field{ field1, field2 };
    defer {
        for (fields) |f| {
            allocator.free(f.seats);
        }
    }

    var idx: usize = 0;
    while (!std.mem.eql(u8, fields[0].seats, fields[1].seats)) {
        const other_idx = 1 - idx;
        var from = &fields[idx];
        var to = &fields[other_idx];

        var row: usize = 0;
        while (row < rows) : (row += 1) {
            var col: usize = 0;
            while (col < cols) : (col += 1) {
                var seat = from.getSeat(row, col, 0, 0).?;
                const deltas = [_]i64{ -1, 0, 1 };
                var occupied: u64 = 0;
                for (deltas) |dr| {
                    for (deltas) |dc| {
                        if (dr == 0 and dc == 0) continue;
                        if (is_occupied(from, row, col, stride, dr, dc)) {
                            occupied += 1;
                        }
                    }
                }
                if (seat == 'L' and occupied == 0) {
                    seat = '#';
                } else if (seat == '#' and occupied >= tolerance) {
                    seat = 'L';
                }
                to.seats[row * stride + col] = seat;
            }
        }

        idx = other_idx;
    }

    return std.mem.count(u8, fields[0].seats, "#");
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "11.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var inputCopy = try allocator.dupe(u8, input);
    defer allocator.free(inputCopy);

    std.debug.print("EASY: {}\n", .{evolve(allocator, input, 4, isOccupiedEasy)});
    std.debug.print("HARD: {}\n", .{evolve(allocator, input, 5, isOccupiedHard)});
}
