const std = @import("std");

const TableElem = struct {
    exp: u64,
    smol_pow: u64,
    big_pow: u64,

    fn less(ctx: void, a: TableElem, b: TableElem) bool {
        return TableElem.cmp(ctx, a, b) == .lt;
    }

    fn cmp(ctx: void, a: TableElem, b: TableElem) std.math.Order {
        return std.math.order(a.smol_pow, b.smol_pow);
    }
};

const G = 7;
const N = 20201227;
const NSQRT = sqrt(N);
const TABLE = getTable(NSQRT);

fn sqrt(comptime n: u64) u64 {
    @setEvalBranchQuota(10000);
    var result: u64 = 0;
    while (result * result < N) {
        result += 1;
    }
    return result;
}

fn pow(comptime x: u64, comptime exp: u64) u64 {
    var result: u64 = 1;
    var to_mul = x;
    var bits = exp;
    while (bits > 0) {
        if ((bits & 1) != 0) {
            result = result * to_mul % N;
        }
        to_mul = to_mul * to_mul % N;
        bits >>= 1;
    }
    return result;
}

fn inverse(comptime x: u64) u64 {
    return pow(x, N - 2);
}

fn partition(comptime table: []TableElem, low: usize, high: usize) usize {
    const pivot = table[high - 1].smol_pow;
    var less_pos = low;
    var idx = low;
    while (idx < high) : (idx += 1) {
        if (table[idx].smol_pow < pivot) {
            std.mem.swap(TableElem, &table[idx], &table[less_pos]);
            less_pos += 1;
        }
    }
    std.mem.swap(TableElem, &table[high - 1], &table[less_pos]);
    return less_pos;
}

fn quicksort(comptime table: []TableElem, low: usize, high: usize) void {
    if (low >= high) {
        return;
    }

    const pivot_idx = partition(table, low, high);
    quicksort(table, low, pivot_idx);
    quicksort(table, pivot_idx + 1, high);
}

fn getTable(comptime n_sqrt: u64) [n_sqrt + 1]TableElem {
    @setEvalBranchQuota(1000000);
    var result = [1]TableElem{std.mem.zeroes(TableElem)} ** (n_sqrt + 1);

    result[0] = TableElem{
        .exp = 0,
        .smol_pow = 1,
        .big_pow = 1,
    };

    const n_sqrt_pow = pow(G, n_sqrt);

    var idx: usize = 1;
    while (idx <= n_sqrt) : (idx += 1) {
        result[idx] = TableElem{
            .exp = idx,
            .smol_pow = result[idx - 1].smol_pow * G % N,
            .big_pow = result[idx - 1].big_pow * n_sqrt_pow % N,
        };
    }
    quicksort(result[0..], 0, result.len);
    return result;
}

fn dlog(comptime x: u64) u64 {
    @setEvalBranchQuota(1000000);
    for (TABLE) |*e, idx| {
        const big_index = NSQRT * e.exp;
        const needle = x * inverse(e.big_pow) % N;
        var low = 0;
        var high = TABLE.len - 1;
        while (low <= high) {
            const mid = (low + high) / 2;
            const cand = TABLE[mid].smol_pow;
            if (cand == needle) {
                return big_index + TABLE[mid].exp;
            } else if (cand < needle) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
    }
    unreachable;
}

fn parseInt(comptime x: []const u8) u64 {
    return std.fmt.parseInt(u64, x, 10) catch unreachable;
}

fn getAnswer(comptime input: []const u8) u64 {
    comptime {
        const newline_idx = std.mem.indexOfScalar(u8, input, '\n').?;
        const a_str = input[0..newline_idx];
        const a = parseInt(a_str);
        const b_str = input[newline_idx + 1 .. input.len - 1];
        const b = parseInt(b_str);
        return pow(a, dlog(b));
    }
}

pub fn main() !void {
    std.debug.print("{}\n", .{getAnswer(@embedFile("25.txt"))});
}
