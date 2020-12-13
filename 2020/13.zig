const std = @import("std");

fn solveEasy(ts: u64, buses: []const ?u64) u64 {
    var best_delay: ?u64 = null;
    var easy_answer: ?u64 = null;
    for (buses) |maybe_bus_ts| {
        if (maybe_bus_ts) |bus_ts| {
            const delay = ((ts + bus_ts - 1) / bus_ts * bus_ts) - ts;
            if (best_delay == null or delay < best_delay.?) {
                best_delay = delay;
                easy_answer = delay * bus_ts;
            }
        }
    }
    return easy_answer.?;
}

fn pow(x: u64, power: u64, mod: u64) u64 {
    if (power == 0) {
        return 1;
    }
    const half_power = pow(x, power / 2, mod);
    var res = half_power * half_power % mod;
    if (power % 2 == 1) {
        res = res * x % mod;
    }
    return res;
}

fn solveHard(buses: []const ?u64) u64 {
    std.debug.assert(buses.len > 0);
    var ans: u64 = 0;
    var prod = buses[0].?;
    for (buses[1..]) |maybe_bus_ts, idx| {
        if (maybe_bus_ts) |bus_ts| {
            const mod_needed = @intCast(u64, @mod(-(@intCast(i64, idx + 1 + ans)), @intCast(i64, bus_ts)));
            const mul = mod_needed * pow(prod, bus_ts - 2, bus_ts) % bus_ts;
            ans += prod * mul;
            prod *= bus_ts;
        }
    }
    return ans;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "13.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var nums = std.ArrayList(?u64).init(allocator);
    defer nums.deinit();

    var it = std.mem.tokenize(input, "\n,");
    const ts = try std.fmt.parseInt(u64, it.next().?, 10);
    while (it.next()) |token| {
        const bus_ts = if (token.len == 1 and token[0] == 'x') null else try std.fmt.parseInt(u64, token, 10);
        try nums.append(bus_ts);
    }
    std.debug.print("EASY: {}\n", .{solveEasy(ts, nums.items)});
    std.debug.print("HARD: {}\n", .{solveHard(nums.items)});
}
