const std = @import("std");

const OpType = enum {
    add,
    mul,
};

const ProblemType = enum {
    easy,
    hard,
};

const NumWithLevel = struct {
    num: ?u64,
    level: u64,
};

fn Solver(problem_type: ProblemType) type {
    return struct {
        const NumType = if (problem_type == .easy) ?u64 else NumWithLevel;
        const Self = @This();

        op_stack: *std.ArrayList(OpType),
        num_stack: *std.ArrayList(NumType),

        fn process_num(self: *Self, num: u64) !void {
            if (problem_type == .easy) {
                if (self.num_stack.pop()) |acc| {
                    const sub_res = switch (self.op_stack.pop()) {
                        .add => acc + num,
                        .mul => acc * num,
                    };
                    try self.num_stack.append(sub_res);
                } else {
                    try self.num_stack.append(num);
                }
            } else {
                const last_num = &self.num_stack.items[self.num_stack.items.len - 1];
                const ops = &self.op_stack.items;
                if (last_num.num != null and (ops.len > 0 and ops.*[ops.len - 1] != .add)) {
                    try self.num_stack.append(NumWithLevel{ .num = num, .level = last_num.level });
                } else {
                    const new_num = if (last_num.num) |acc| blk: {
                        const op = self.op_stack.pop();
                        std.debug.assert(op == .add);
                        break :blk acc + num;
                    } else num;
                    _ = self.num_stack.pop();
                    try self.num_stack.append(NumWithLevel{ .num = new_num, .level = last_num.level });
                }
            }
        }

        fn increaseLevel(self: *Self) !void {
            try self.num_stack.append(if (problem_type == .easy) null else blk: {
                const nums = &self.num_stack.items;
                const next_level = if (nums.len == 0) 0 else nums.*[nums.len - 1].level + 1;
                break :blk NumWithLevel{ .num = null, .level = next_level };
            });
        }

        fn solve(self: *Self, input: []const u8) !u64 {
            var result: u64 = 0;

            var input_left = input;
            while (input_left.len > 0) {
                const end_pos = std.mem.indexOfScalar(u8, input_left, '\n').? + 1;
                const line = input_left[0..end_pos];

                self.op_stack.shrinkRetainingCapacity(0);
                self.num_stack.shrinkRetainingCapacity(0);
                try self.increaseLevel();
                for (line) |ch, idx| {
                    switch (ch) {
                        ' ' => {},
                        '+' => try self.op_stack.append(.add),
                        '*' => try self.op_stack.append(.mul),
                        '0'...'9' => try self.process_num(@intCast(u64, ch - '0')),
                        '(' => try self.increaseLevel(),
                        ')', '\n' => {
                            var last_num = self.num_stack.pop();
                            const num_to_process = if (problem_type == .easy) last_num.? else blk: {
                                const stk = &self.num_stack.items;
                                while (stk.len > 0 and stk.*[stk.len - 1].level == last_num.level) {
                                    const op = self.op_stack.pop();
                                    std.debug.assert(op == .mul);
                                    last_num.num.? *= self.num_stack.pop().num.?;
                                }
                                break :blk last_num.num.?;
                            };
                            if (self.num_stack.items.len == 0) {
                                try self.num_stack.append(last_num);
                            } else {
                                try self.process_num(num_to_process);
                            }
                        },
                        else => unreachable,
                    }
                }

                std.debug.assert(self.num_stack.items.len == 1);
                std.debug.assert(self.op_stack.items.len == 0);

                const ans = self.num_stack.pop();
                result += if (problem_type == .easy) ans.? else ans.num.?;

                input_left = input_left[end_pos..];
            }

            return result;
        }
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var op_stack = std.ArrayList(OpType).init(allocator);
    defer op_stack.deinit();

    var input = try std.fs.cwd().readFileAlloc(allocator, "18.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    const type_descs = [_][]const u8{ "EASY", "HARD" };
    const types = [_]ProblemType{ .easy, .hard };
    inline for (types) |t, idx| {
        var num_stack = std.ArrayList(Solver(t).NumType).init(allocator);
        defer num_stack.deinit();

        var solver = Solver(t){
            .op_stack = &op_stack,
            .num_stack = &num_stack,
        };
        std.debug.print("{}: {}\n", .{ type_descs[idx], solver.solve(input) });
    }
}
