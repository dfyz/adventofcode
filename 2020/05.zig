const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var input = try std.fs.cwd().openFile("05.txt", .{ .read = true });
    var reader = input.reader();
    var pass: u64 = 0;
    var max_pass: u64 = 0;
    var all_passes = std.ArrayList(u64).init(&gpa.allocator);
    defer all_passes.deinit();

    while (true) {
        const ch = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        switch (ch) {
            'B', 'R' => pass = pass * 2 + 1,
            'F', 'L' => pass *= 2,
            '\n' => {
                if (pass > max_pass) {
                    max_pass = pass;
                }
                try all_passes.append(pass);
                pass = 0;
            },
            else => unreachable,
        }
    }

    std.debug.print("EASY: {}\n", .{max_pass});

    std.sort.sort(u64, all_passes.items, {}, comptime std.sort.asc(u64));
    for (all_passes.items) |p, idx| {
        // Assuming the input is correct, we shouldn't trigger the "index out of bounds" error.
        if (p + 1 != all_passes.items[idx + 1]) {
            std.debug.print("HARD: {}\n", .{p + 1});
            break;
        }
    }
}
