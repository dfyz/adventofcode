const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;
    defer _ = gpa.deinit();

    var line = std.ArrayList(u8).init(allocator);
    defer line.deinit();

    var input = try std.fs.cwd().openFile("06.txt", .{ .read = true });
    var reader = input.reader();

    var easy_answers = [_]bool{false} ** 26;
    var hard_answers = [_]bool{true} ** 26;

    var easy_answer: u64 = 0;
    var hard_answer: u64 = 0;

    while (true) {
        var end_of_stream = false;

        reader.readUntilDelimiterArrayList(&line, '\n', std.math.maxInt(usize)) catch |err| switch (err) {
            error.EndOfStream => end_of_stream = true,
            else => return err,
        };

        if (end_of_stream or line.items.len == 0) {
            for (easy_answers) |val| {
                easy_answer += @boolToInt(val);
            }
            for (hard_answers) |val| {
                hard_answer += @boolToInt(val);
            }
            std.mem.set(bool, easy_answers[0..], false);
            std.mem.set(bool, hard_answers[0..], true);
        } else {
            var answers = [_]bool{false} ** 26;
            for (line.items) |ch| {
                const idx = ch - 'a';
                answers[idx] = true;
                easy_answers[idx] = true;
            }
            for (answers) |val, idx| {
                if (!val) {
                    hard_answers[idx] = false;
                }
            }
        }

        if (end_of_stream) break;
    }

    std.debug.print("EASY: {}\n", .{easy_answer});
    std.debug.print("HARD: {}\n", .{hard_answer});
}
