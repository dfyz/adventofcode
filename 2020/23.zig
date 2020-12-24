const std = @import("std");

const N = 9;

fn cycle(digit: u64) u64 {
    return if (digit == 1) N else digit - 1;
}

fn to_digits(num: u64) [N]u64 {
    var result = [_]u64{0} ** N;
    var idx: usize = 0;
    var tmp_num = num;
    while (idx < result.len) : (idx += 1) {
        result[N - 1 - idx] = tmp_num % 10;
        tmp_num /= 10;
    }
    return result;
}

fn move(num: u64) u64 {
    var digits = to_digits(num);
    const skip = 3;

    const current = digits[0];
    const a = digits[skip - 2];
    const b = digits[skip - 1];
    const c = digits[skip];

    var next = cycle(current);
    var insert_pos: ?usize = null;
    while (insert_pos == null) : (next = cycle(next)) {
        insert_pos = std.mem.indexOfScalar(u64, digits[skip + 1 ..], next);
        if (insert_pos) |*pos| {
            pos.* += skip + 1;
        }
    }

    var result: u64 = 0;
    var idx: usize = skip + 1;
    while (idx < digits.len) : (idx += 1) {
        // std.debug.print("{} {} {}\n", .{ idx, insert_pos, result });
        result = result * 10 + digits[idx];
        if (idx == insert_pos) {
            result = result * 1000 + a * 100 + b * 10 + c;
        }
    }
    result = result * 10 + current;
    return result;
}

test "move" {
    const expect = @import("std").testing.expect;

    const nums = [_]u64{
        389125467,
        289154673,
        546789132,
        891346725,
        467913258,
        136792584,
        936725841,
        258367419,
        674158392,
        574183926,
        837419265,
    };

    for (nums) |n, idx| {
        if (idx + 1 < nums.len) {
            expect(move(nums[idx]) == nums[idx + 1]);
        }
    }
}

fn to_answer(num: u64) u64 {
    const digits = to_digits(num);
    const pos1 = std.mem.indexOfScalar(u64, digits[0..], 1).?;
    var result: u64 = 0;
    var idx: usize = pos1 + 1;
    while (idx < digits.len) : (idx += 1) {
        result = result * 10 + digits[idx];
    }
    idx = 0;
    while (idx < pos1) : (idx += 1) {
        result = result * 10 + digits[idx];
    }
    return result;
}

pub fn main() !void {
    var num: u64 = try std.fmt.parseInt(u64, @embedFile("23.txt"), 10);
    var idx: usize = 0;
    while (idx < 100) : (idx += 1) {
        num = move(num);
    }

    std.debug.print("EASY: {}\n", .{to_answer(num)});
}
