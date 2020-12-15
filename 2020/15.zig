const std = @import("std");

pub fn solve(nums: *const std.ArrayList(u64), steps: u64, allocator: *std.mem.Allocator) !u64 {
    var prev_pos = std.AutoHashMap(u64, u64).init(allocator);
    defer prev_pos.deinit();

    var current_num: u64 = std.math.maxInt(u64);
    var step: u64 = 0;
    while (step < steps) : (step += 1) {
        const prev_num = current_num;
        if (prev_pos.get(prev_num)) |pos| {
            current_num = step - pos;
        } else {
            current_num = if (step >= nums.items.len) 0 else nums.items[step];
        }
        try prev_pos.put(prev_num, step);
    }
    return current_num;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    const input = try std.fs.cwd().readFileAlloc(allocator, "15.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var nums = std.ArrayList(u64).init(allocator);
    defer nums.deinit();

    var it = std.mem.tokenize(input, ",\n");
    while (it.next()) |token| {
        try nums.append(try std.fmt.parseInt(u64, token, 10));
    }

    std.debug.print("EASY: {}\n", .{solve(&nums, 2020, allocator)});
    std.debug.print("HARD: {}\n", .{solve(&nums, 30_000_000, allocator)});
}
