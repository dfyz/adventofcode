const std = @import("std");

const RuleType = enum(usize) {
    terminal,
    nonterminal,
    rule8,
    wildcard,
};

const Children = [2]usize;

const Rule = union(RuleType) {
    terminal: u8,
    nonterminal: [2]Children,
    rule8: [2]Children,
    wildcard: void,
};

const RawRule = struct {
    index: usize,
    rule_str: []const u8,

    fn cmp(context: void, lhs: RawRule, rhs: RawRule) bool {
        return lhs.index < rhs.index;
    }
};

fn intOrDie(str: []const u8) usize {
    return std.fmt.parseInt(usize, str, 10) catch unreachable;
}

fn childrenOrDie(it: *std.mem.TokenIterator) Children {
    return Children{ intOrDie(it.next().?), intOrDie(it.next().?) };
}

fn ruleOrDie(rule: *const RawRule) Rule {
    if (rule.index == 8) {
        const dummy_children = Children{ 42, 8 };
        return Rule{ .rule8 = [_]Children{ dummy_children, dummy_children } };
    }
    if (std.mem.indexOfScalar(u8, rule.rule_str, '"') != null) {
        return Rule{ .terminal = rule.rule_str[1] };
    }
    const space_count = std.mem.count(u8, rule.rule_str, " ");
    const has_pipe = std.mem.indexOfScalar(u8, rule.rule_str, '|') != null;
    var it = std.mem.tokenize(rule.rule_str, " |");
    if (!has_pipe) {
        std.debug.assert(space_count == 1);
        const children = childrenOrDie(&it);
        return Rule{
            .nonterminal = [_]Children{
                children,
                children,
            },
        };
    } else {
        if (space_count == 2) {
            return Rule.wildcard;
        } else {
            const children1 = childrenOrDie(&it);
            const children2 = childrenOrDie(&it);
            return Rule{
                .nonterminal = [_]Children{
                    children1,
                    children2,
                },
            };
        }
    }
}

fn Array3D(comptime T: type) type {
    return struct {
        arr: []T,
        d1: usize,
        d2: usize,
        d3: usize,

        const Self = @This();

        fn init(allocator: *std.mem.Allocator, d1: usize, d2: usize, d3: usize) !Self {
            return Self{
                .arr = try allocator.alloc(T, d1 * d2 * d3),
                .d1 = d1,
                .d2 = d2,
                .d3 = d3,
            };
        }

        fn get(self: *const Self, idx1: usize, idx2: usize, idx3: usize) T {
            return self.arr[self.to_pos(idx1, idx2, idx3)];
        }

        fn set(self: *const Self, idx1: usize, idx2: usize, idx3: usize, val: T) void {
            self.arr[self.to_pos(idx1, idx2, idx3)] = val;
        }

        fn to_pos(self: *const Self, idx1: usize, idx2: usize, idx3: usize) usize {
            return idx3 + idx2 * self.d3 + idx1 * self.d2 * self.d3;
        }
    };
}

const ProblemType = enum {
    easy,
    hard,
};

fn solve(allocator: *std.mem.Allocator, input: []const u8, problem_type: ProblemType) !u64 {
    var line_it = std.mem.split(input, "\n");
    var rules_read = false;

    var raw_rules = std.ArrayList(RawRule).init(allocator);
    while (line_it.next()) |line| {
        if (line.len == 0) {
            break;
        }

        const index = intOrDie(line[0..std.mem.indexOfScalar(u8, line, ':').?]);
        const rule_proper = line[std.mem.indexOfScalar(u8, line, ' ').? + 1 ..];
        try raw_rules.append(RawRule{ .index = index, .rule_str = rule_proper });
    }

    std.sort.sort(RawRule, raw_rules.items, {}, RawRule.cmp);

    var rules = std.ArrayList(Rule).init(allocator);
    for (raw_rules.items) |*pr| {
        try rules.append(ruleOrDie(pr));
    }

    var result: u64 = 0;
    while (line_it.next()) |line| {
        if (line.len == 0) {
            break;
        }

        var can_match = try Array3D(bool).init(allocator, line.len, line.len + 1, rules.items.len);
        std.mem.set(bool, can_match.arr, false);

        var len: usize = 1;
        while (len <= line.len) : (len += 1) {
            var start: usize = 0;
            while (start + len <= line.len) : (start += 1) {
                var raw_rule_idx: usize = 0;
                while (raw_rule_idx < rules.items.len) : (raw_rule_idx += 1) {
                    const idx = rules.items.len - 1 - raw_rule_idx;
                    const rule = rules.items[idx];

                    if (switch (rule) {
                        .terminal => |t| len == 1 and line[start] == t,
                        .nonterminal, .rule8 => |nt| blk: {
                            if (rule == .rule8 and can_match.get(start, len, 42)) {
                                break :blk true;
                            }

                            if (rule != .rule8 or problem_type == .hard) {
                                var len1: usize = 1;
                                while (len1 < len) : (len1 += 1) {
                                    const len2 = len - len1;
                                    if (can_match.get(start, len1, nt[0][0]) and can_match.get(start + len1, len2, nt[0][1])) {
                                        break :blk true;
                                    }
                                    if (can_match.get(start, len1, nt[1][0]) and can_match.get(start + len1, len2, nt[1][1])) {
                                        break :blk true;
                                    }
                                }
                            }

                            if (idx == 11 and problem_type == .hard) {
                                var len1: usize = 1;
                                while (len1 < len) : (len1 += 1) {
                                    var len2: usize = 1;
                                    while (len1 + len2 < len) : (len2 += 1) {
                                        const len3 = len - len1 - len2;
                                        if (can_match.get(start, len1, 42) and can_match.get(start + len1, len2, 11) and can_match.get(start + len1 + len2, len3, 31)) {
                                            break :blk true;
                                        }
                                    }
                                }
                            }
                            break :blk false;
                        },
                        .wildcard => len == 1,
                    }) {
                        can_match.set(start, len, idx, true);
                    }
                }
            }
        }

        result += @boolToInt(can_match.get(0, line.len, 0));
    }
    return result;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var allocator = &arena.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "19.txt", std.math.maxInt(usize));

    std.debug.print("EASY: {}\n", .{try solve(allocator, input, .easy)});
    std.debug.print("HARD: {}\n", .{try solve(allocator, input, .hard)});
}
