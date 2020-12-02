const std = @import("std");

const Target = 2020;

fn solve(nums: *std.ArrayList(u64), comptime find_triple: bool) ?u64 {
    var idx1: usize = 0;
    while (idx1 < nums.items.len) : (idx1 += 1) {
        var idx2: usize = idx1 + 1;
        while (idx2 < nums.items.len) : (idx2 += 1) {
            const item1 = nums.items[idx1];
            const item2 = nums.items[idx2];

            if (find_triple) {
                var idx3 = idx2 + 1;
                while (idx3 < nums.items.len) : (idx3 += 1) {
                    const item3 = nums.items[idx3];
                    if (item1 + item2 + item3 == 2020) {
                        return item1 * item2 * item3;
                    }
                }
            } else {
                if (item1 + item2 == 2020) {
                    return item1 * item2;
                }
            }
        }
    }
    return null;
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        if (alloc.deinit()) {
            std.debug.print("leaked memory!", .{});
        }
    }

    var nums = std.ArrayList(u64).init(&alloc.allocator);
    defer nums.deinit();

    const input = try std.fs.cwd().openFile("01.txt", .{
        .read = true,
    });
    defer input.close();

    var bufReader = std.io.bufferedReader(input.reader());
    var reader = bufReader.reader();

    var num: u64 = 0;
    while (true) {
        const b = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        if (b == '\n') {
            try nums.append(num);
            num = 0;
        } else {
            num = num * 10 + try std.fmt.charToDigit(b, 10);
        }
    }

    std.debug.assert(num == 0);

    std.debug.print("EASY: {}\n", .{solve(&nums, false)});
    std.debug.print("HARD: {}\n", .{solve(&nums, true)});
}
