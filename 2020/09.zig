const std = @import("std");

const nPrev = 25;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "09.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var it = std.mem.tokenize(input, "\n");
    var nums = std.ArrayList(u64).init(allocator);
    defer nums.deinit();
    while (it.next()) |line| {
        try nums.append(try std.fmt.parseInt(u64, line, 10));
    }

    var cur: usize = nPrev;
    var easy_answer: ?u64 = null;
    while (cur < nums.items.len) : (cur += 1) {
        const target = nums.items[cur];
        var found = false;
        var num1 = cur - nPrev;
        outer: while (num1 < cur) : (num1 += 1) {
            var num2 = num1 + 1;
            while (num2 < cur) : (num2 += 1) {
                if (nums.items[num1] + nums.items[num2] == target) {
                    found = true;
                    break :outer;
                }
            }
        }
        if (!found) {
            easy_answer = target;
            break;
        }
    }

    std.debug.print("EASY: {}\n", .{easy_answer});
    if (easy_answer) |target| {
        var num1: usize = 0;
        var num2: usize = 0;
        var sum: u64 = nums.items[0];
        outer: while (num1 < nums.items.len) {
            while (num2 + 1 < nums.items.len and sum + nums.items[num2 + 1] <= target) {
                sum += nums.items[num2 + 1];
                num2 += 1;
            }
            if (sum == target) {
                const target_nums = nums.items[num1 .. num2 + 1];
                const min = std.sort.min(u64, target_nums, {}, comptime std.sort.asc(u64)).?;
                const max = std.sort.max(u64, target_nums, {}, comptime std.sort.asc(u64)).?;
                const hard_answer = min + max;
                std.debug.print("HARD: {}\n", .{hard_answer});
                break :outer;
            }
            sum -= nums.items[num1];
            num1 += 1;
            if (num2 < num1) {
                num2 += 1;
            }
        }
    }
}
