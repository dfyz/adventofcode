const std = @import("std");

const SubBag = struct {
    count: u64,
    index: usize,
};

fn canReachShinyGold(
    idx: usize,
    shiny_gold_idx: usize,
    sub_bags: []const SubBag,
    bag_starts: []const usize,
    can_reach: []?bool,
) bool {
    if (can_reach[idx]) |val| {
        return val;
    }

    var result = idx == shiny_gold_idx;
    var sub_bag_idx = bag_starts[idx];
    const sub_bag_end = if (idx + 1 < bag_starts.len) bag_starts[idx + 1] else sub_bags.len;
    while (sub_bag_idx < sub_bag_end) : (sub_bag_idx += 1) {
        const sub_bag = sub_bags[sub_bag_idx];
        if (canReachShinyGold(sub_bag.index, shiny_gold_idx, sub_bags, bag_starts, can_reach)) {
            result = true;
            break;
        }
    }
    can_reach[idx] = result;
    return result;
}

fn subBagCount(idx: usize, sub_bags: []const SubBag, bag_starts: []const usize, sub_bag_counts: []?u64) u64 {
    if (sub_bag_counts[idx]) |val| {
        return val;
    }

    var result: u64 = 0;
    var sub_bag_idx = bag_starts[idx];
    const sub_bag_end = if (idx + 1 < bag_starts.len) bag_starts[idx + 1] else sub_bags.len;
    while (sub_bag_idx < sub_bag_end) : (sub_bag_idx += 1) {
        const sub_bag = sub_bags[sub_bag_idx];
        result += sub_bag.count * (1 + subBagCount(sub_bag.index, sub_bags, bag_starts, sub_bag_counts));
    }
    sub_bag_counts[idx] = result;
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;
    defer _ = gpa.deinit();

    var input = try std.fs.cwd().readFileAlloc(allocator, "07.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var color_to_idx = std.StringHashMap(u64).init(allocator);
    defer color_to_idx.deinit();

    var it = std.mem.tokenize(input, "\n");
    while (it.next()) |line| {
        const color_end = std.mem.indexOf(u8, line, " bags").?;
        const key = line[0..color_end];
        try color_to_idx.put(key, color_to_idx.count());
    }

    const bag_count = color_to_idx.count();

    var sub_bags = std.ArrayList(SubBag).init(allocator);
    defer sub_bags.deinit();

    var bag_starts = try allocator.alloc(usize, bag_count);
    defer allocator.free(bag_starts);

    it = std.mem.tokenize(input, "\n");
    var line_idx: usize = 0;
    while (it.next()) |line| {
        bag_starts[line_idx] = sub_bags.items.len;

        var word_it = std.mem.tokenize(line, " ");
        while (word_it.next()) |word| {
            if (std.fmt.parseInt(u64, word, 10)) |count| {
                const rest = word_it.rest();
                const color_end = std.mem.indexOf(u8, rest, " bag").?;
                const key = word_it.rest()[0..color_end];
                try sub_bags.append(SubBag{
                    .count = count,
                    .index = color_to_idx.get(key).?,
                });
            } else |err| {}
        }

        line_idx += 1;
    }

    const shiny_gold_idx = color_to_idx.get("shiny gold").?;
    var can_reach = try allocator.alloc(?bool, bag_count);
    defer allocator.free(can_reach);
    std.mem.set(?bool, can_reach, null);

    var easy_answer: u64 = 0;
    for (bag_starts) |_, idx| {
        if (canReachShinyGold(idx, shiny_gold_idx, sub_bags.items, bag_starts, can_reach)) {
            easy_answer += 1;
        }
    }
    // Account for the shiny gold bag proper.
    easy_answer -= 1;
    std.debug.print("EASY: {}\n", .{easy_answer});

    var sub_bag_counts = try allocator.alloc(?u64, bag_count);
    defer allocator.free(sub_bag_counts);
    std.mem.set(?u64, sub_bag_counts, null);

    const hard_answer = subBagCount(shiny_gold_idx, sub_bags.items, bag_starts, sub_bag_counts);
    std.debug.print("HARD: {}\n", .{hard_answer});
}
