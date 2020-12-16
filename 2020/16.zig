const std = @import("std");

const Range = struct {
    min: u64,
    max: u64,

    fn contains(self: *const Range, num: u64) bool {
        return num >= self.min and num <= self.max;
    }

    fn init(desc: []const u8) !Range {
        var it = std.mem.tokenize(desc, "-");
        return Range{
            .min = try std.fmt.parseInt(u64, it.next().?, 10),
            .max = try std.fmt.parseInt(u64, it.next().?, 10),
        };
    }
};

const Field = struct {
    range1: Range,
    range2: Range,
    is_departure: bool,

    fn contains(self: *const Field, num: u64) bool {
        return self.range1.contains(num) or self.range2.contains(num);
    }
};

fn readTicket(line: []const u8, tickets: *std.ArrayList(u64)) !void {
    var token_it = std.mem.tokenize(line, ",");
    while (token_it.next()) |token| {
        try tickets.append(try std.fmt.parseInt(u64, token, 10));
    }
}

fn readField(line: []const u8) !Field {
    var token_it = std.mem.tokenize(line, ": ");
    var is_departure = false;
    var range1: ?Range = null;
    var range2: ?Range = null;
    while (token_it.next()) |token| {
        if (std.mem.indexOf(u8, token, "-")) |_| {
            var range_ptr = if (range1 != null) &range2 else &range1;
            range_ptr.* = try Range.init(token);
        } else if (std.mem.eql(u8, token, "departure")) {
            is_departure = true;
        }
    }
    return Field{
        .range1 = range1.?,
        .range2 = range2.?,
        .is_departure = is_departure,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "16.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var fields = std.ArrayList(Field).init(allocator);
    defer fields.deinit();

    var my_ticket = std.ArrayList(u64).init(allocator);
    defer my_ticket.deinit();

    var nearby_tickets = std.ArrayList(u64).init(allocator);
    defer nearby_tickets.deinit();

    var tickets = std.ArrayList([]const u64).init(allocator);

    var line_it = std.mem.tokenize(input, "\n");
    var reading_my_ticket = false;
    var reading_nearby_tickets = false;
    var easy_answer: u64 = 0;
    while (line_it.next()) |line| {
        if (std.mem.eql(u8, line, "your ticket:")) {
            reading_my_ticket = true;
        } else if (std.mem.eql(u8, line, "nearby tickets:")) {
            reading_nearby_tickets = true;
        } else {
            if (std.mem.indexOf(u8, line, " or ")) |_| {
                try fields.append(try readField(line));
            } else if (reading_my_ticket and !reading_nearby_tickets) {
                try readTicket(line, &my_ticket);
            } else if (reading_nearby_tickets) {
                try readTicket(line, &nearby_tickets);
            }
        }
    }

    std.debug.assert(nearby_tickets.items.len % fields.items.len == 0);
    const nearby_ticket_count = nearby_tickets.items.len / fields.items.len;

    var valid_tickets = try allocator.alloc(bool, nearby_ticket_count);
    defer allocator.free(valid_tickets);
    std.mem.set(bool, valid_tickets, true);

    for (nearby_tickets.items) |num, num_idx| {
        var valid = false;
        outer: for (fields.items) |f| {
            if (f.contains(num)) {
                valid = true;
                break :outer;
            }
        }
        if (!valid) {
            valid_tickets[num_idx / fields.items.len] = false;
            easy_answer += num;
        }
    }

    var match_mask = try allocator.alloc(u64, fields.items.len);
    defer allocator.free(match_mask);
    std.mem.set(u64, match_mask, 0);

    var match_found = try allocator.alloc(bool, fields.items.len);
    defer allocator.free(match_found);
    std.mem.set(bool, match_found, false);

    var pos_idx: usize = 0;
    while (pos_idx < fields.items.len) : (pos_idx += 1) {
        for (fields.items) |f, field_idx| {
            var matches = true;
            var ticket_idx: usize = 0;
            while (ticket_idx < nearby_ticket_count) : (ticket_idx += 1) {
                if (!valid_tickets[ticket_idx]) continue;
                const num = nearby_tickets.items[ticket_idx * fields.items.len + pos_idx];
                if (!f.contains(num)) {
                    matches = false;
                    break;
                }
            }
            if (matches) {
                match_mask[pos_idx] |= @intCast(u64, 1) << @intCast(u6, field_idx);
            }
        }
    }

    var match_idx: usize = 0;
    while (match_idx < fields.items.len) : (match_idx += 1) {
        for (match_mask) |mask, mask_pos_idx| {
            if (!match_found[mask_pos_idx] and @popCount(u64, mask) == 1) {
                match_found[mask_pos_idx] = true;
                for (match_mask) |*other_mask, other_mask_pos_idx| {
                    if (mask_pos_idx != other_mask_pos_idx) {
                        other_mask.* &= ~mask;
                    }
                }
                break;
            }
        } else {
            std.debug.panic("no match found on iteration {}", .{match_idx});
        }
    }

    var hard_answer: u64 = 1;
    for (my_ticket.items) |num, ticket_pos_idx| {
        const field_idx = @ctz(u64, match_mask[ticket_pos_idx]);
        if (fields.items[field_idx].is_departure) {
            hard_answer *= num;
        }
    }

    std.debug.print("EASY: {}\n", .{easy_answer});
    std.debug.print("HARD: {}\n", .{hard_answer});
}
