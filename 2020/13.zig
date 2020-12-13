const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "13.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var it = std.mem.tokenize(input, "\n,");
    const ts = try std.fmt.parseInt(u64, it.next().?, 10);
    var best_delay: ?u64 = null;
    var easy_answer: ?u64 = null;
    while (it.next()) |token| {
        if (token.len == 1 and token[0] == 'x') continue;
        const bus_ts = try std.fmt.parseInt(u64, token, 10);
        const delay = ((ts + bus_ts - 1) / bus_ts * bus_ts) - ts;
        if (best_delay == null or delay < best_delay.?) {
            best_delay = delay;
            easy_answer = delay * bus_ts;
        }
    }
    std.debug.print("{}\n", .{easy_answer});
}
