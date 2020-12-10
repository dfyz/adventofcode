const std = @import("std");

fn count_ways(start: usize, nums: []u64, ways: []?u64) u64 {
    if (ways[start]) |val| return val;

    const result: u64 = if (start + 1 >= nums.len)
        1
    else blk: {
        var sub_ways: u64 = 0;
        var next_start = start + 1;
        while (next_start < nums.len and nums[next_start] - nums[start] <= 3) : (next_start += 1) {
            sub_ways += count_ways(next_start, nums, ways);
        }
        break :blk sub_ways;
    };

    ways[start] = result;
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;
    defer _ = gpa.deinit();

    var input = try std.fs.cwd().readFileAlloc(allocator, "10.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var nums = std.ArrayList(u64).init(allocator);
    defer nums.deinit();

    var it = std.mem.tokenize(input, "\n");
    while (it.next()) |line| {
        try nums.append(try std.fmt.parseInt(u64, line, 10));
    }

    try nums.append(0);
    std.sort.sort(u64, nums.items, {}, comptime std.sort.asc(u64));
    try nums.append(nums.items[nums.items.len - 1] + 3);

    var idx: u64 = 0;
    var diffs = [_]u64{0} ** 4;
    while (idx + 1 < nums.items.len) : (idx += 1) {
        const diff = nums.items[idx + 1] - nums.items[idx];
        diffs[diff] += 1;
    }

    var easy_answer = diffs[1] * diffs[3];
    std.debug.print("EASY: {}\n", .{easy_answer});

    var ways = try allocator.alloc(?u64, nums.items.len);
    std.mem.set(?u64, ways, null);
    defer allocator.free(ways);

    const hard_answer = count_ways(0, nums.items, ways);
    std.debug.print("HARD: {}\n", .{hard_answer});
}
