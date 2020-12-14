const std = @import("std");

const MemSize = 1 << 16;
const BitCount = 36;

const RunType = enum {
    easy,
    hard,
};

const Mem = std.AutoHashMap(u64, u64);

fn setAddrs(floating_bits: u64, mem: *Mem, addr: u64, val: u64) error{OutOfMemory}!void {
    if (floating_bits == 0) {
        try mem.put(addr, val);
    } else {
        const next_floating_bits = floating_bits & (floating_bits - 1);
        const current_bit = floating_bits & ~(floating_bits - 1);
        try setAddrs(next_floating_bits, mem, addr | current_bit, val);
        try setAddrs(next_floating_bits, mem, addr & ~current_bit, val);
    }
}

fn run(input: []u8, mem: *Mem, run_type: RunType) !u64 {
    mem.clearRetainingCapacity();

    var bits_to_set: u64 = 0;
    var bits_to_clear: u64 = 0;
    var floating_bits: u64 = 0;

    var it = std.mem.tokenize(input, "\n[]= ");
    while (it.next()) |token| {
        if (std.mem.eql(u8, token, "mask")) {
            bits_to_set = 0;
            bits_to_clear = 0;
            floating_bits = 0;

            const mask = it.next().?;
            std.debug.assert(mask.len == BitCount);
            var idx: usize = 0;
            var current_bit: u64 = 1;
            while (idx < BitCount) : ({
                idx += 1;
                current_bit <<= 1;
            }) {
                switch (mask[BitCount - 1 - idx]) {
                    'X' => floating_bits |= current_bit,
                    '0' => bits_to_clear |= current_bit,
                    '1' => bits_to_set |= current_bit,
                    else => unreachable,
                }
            }
        } else {
            const addr = try std.fmt.parseInt(u64, it.next().?, 10);
            const val = try std.fmt.parseInt(u64, it.next().?, 10);
            try switch (run_type) {
                .easy => mem.put(addr, val | bits_to_set & ~bits_to_clear),
                .hard => setAddrs(floating_bits, mem, addr | bits_to_set, val),
            };
        }
    }

    var result: u64 = 0;
    var mem_it = mem.iterator();
    while (mem_it.next()) |kv| {
        result += kv.value;
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    const input = try std.fs.cwd().readFileAlloc(allocator, "14.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var mem = Mem.init(allocator);
    defer mem.deinit();

    std.debug.print("EASY: {}\n", .{run(input, &mem, RunType.easy)});
    std.debug.print("HARD: {}\n", .{run(input, &mem, RunType.hard)});
}
