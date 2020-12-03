const std = @import("std");

const Field = struct {
    field: []const u8,
    rows: usize,
    cols: usize,

    fn init(input: []const u8) Field {
        const cols = std.mem.indexOfScalar(u8, input, '\n').?;
        std.debug.assert(input.len % (cols + 1) == 0);
        return .{
            .field = input,
            .rows = input.len / (cols + 1),
            .cols = cols,
        };
    }

    fn solveEasy(self: *const Field, dr: usize, dc: usize) u64 {
        var result: u64 = 0;
        var r: usize = 0;
        var c: usize = 0;
        while (r < self.rows) : (r += dr) {
            if (self.getChar(r, c) == '#') {
                result += 1;
            }
            c = (c + dc) % self.cols;
        }
        return result;
    }

    fn solveHard(self: *const Field) u64 {
        const drs = [_]usize{ 1, 1, 1, 1, 2 };
        const dcs = [_]usize{ 1, 3, 5, 7, 1 };
        var result: u64 = 1;
        for (drs) |dr, idx| {
            result *= self.solveEasy(dr, dcs[idx]);
        }
        return result;
    }

    fn getChar(self: *const Field, r: usize, c: usize) u8 {
        return self.field[r * (self.cols + 1) + c];
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;
    defer std.debug.assert(!gpa.deinit());

    const input = try std.fs.cwd().readFileAlloc(allocator, "03.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    const field = Field.init(input);

    std.debug.print("EASY: {}\n", .{field.solveEasy(1, 3)});
    std.debug.print("HARD: {}\n", .{field.solveHard()});
}
