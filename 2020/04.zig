const std = @import("std");

const Record = std.StringHashMap([]const u8);

fn readLine(comptime T: type, reader: *T, line: *std.ArrayList(u8), at_record_end: *bool, at_stream_end: *bool) !void {
    at_stream_end.* = false;
    at_record_end.* = false;
    reader.readUntilDelimiterArrayList(line, '\n', std.math.maxInt(usize)) catch |err| switch (err) {
        error.EndOfStream => {
            at_record_end.* = true;
            at_stream_end.* = true;
        },
        else => return err,
    };
    if (line.items.len == 0 or (line.items.len == 1 and line.items[0] == '\n')) {
        at_record_end.* = true;
    }
}

const Validator = fn ([]const u8) bool;

const Field = struct {
    name: []const u8,
    validator: Validator,
};

fn checkNum(comptime min: u64, comptime max: u64) Validator {
    return struct {
        fn validate(val: []const u8) bool {
            const num = std.fmt.parseInt(u64, val, 10) catch |_| return false;
            return num >= min and num <= max;
        }
    }.validate;
}

fn checkHeight(val: []const u8) bool {
    if (val.len < 2) return false;

    const prefix = val[0 .. val.len - 2];
    const suffix = val[val.len - 2 ..];
    if (std.mem.eql(u8, suffix, "cm")) {
        return checkNum(150, 193)(prefix);
    } else if (std.mem.eql(u8, suffix, "in")) {
        return checkNum(59, 76)(prefix);
    }
    return false;
}

fn checkHairColor(val: []const u8) bool {
    if (val.len != 7 or val[0] != '#') return false;
    return if (std.fmt.parseInt(u64, val[1..], 16)) |_| true else |_| false;
}

fn checkEyeColor(val: []const u8) bool {
    const validColors = [_][]const u8{
        "amb",
        "blu",
        "brn",
        "gry",
        "grn",
        "hzl",
        "oth",
    };
    for (validColors) |c| {
        if (std.mem.eql(u8, val, c)) {
            return true;
        }
    }
    return false;
}

fn checkPassport(val: []const u8) bool {
    if (val.len != 9) return false;
    return checkNum(0, 999_999_999)(val);
}

const requiredFields = [_]Field{
    .{ .name = "byr", .validator = checkNum(1920, 2002) },
    .{ .name = "iyr", .validator = checkNum(2010, 2020) },
    .{ .name = "eyr", .validator = checkNum(2020, 2030) },
    .{ .name = "hgt", .validator = checkHeight },
    .{ .name = "hcl", .validator = checkHairColor },
    .{ .name = "ecl", .validator = checkEyeColor },
    .{ .name = "pid", .validator = checkPassport },
};

fn check(record: *const Record, valid_easy: *bool, valid_hard: *bool) void {
    for (requiredFields) |field| {
        if (record.get(field.name)) |val| {
            if (!field.validator(val)) {
                valid_hard.* = false;
            }
        } else {
            valid_easy.* = false;
            valid_hard.* = false;
        }
    }
}

fn checkHard(record: *const Record) bool {}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = &gpa.allocator;
    defer std.debug.assert(!gpa.deinit());

    var input = try std.fs.cwd().openFile("04.txt", .{});
    var reader = input.reader();
    var line = std.ArrayList(u8).init(allocator);

    var record = std.hash_map.StringHashMap([]const u8).init(allocator);
    defer record.deinit();

    var easy_result: u64 = 0;
    var hard_result: u64 = 0;

    var at_stream_end = false;
    while (!at_stream_end) {
        var at_record_end = false;
        try readLine(@TypeOf(reader), &reader, &line, &at_record_end, &at_stream_end);

        if (!at_record_end) {
            var fieldIt = std.mem.split(line.items, " ");
            while (fieldIt.next()) |field| {
                var kvIt = std.mem.split(field, ":");
                var k = try allocator.dupe(u8, kvIt.next().?);
                var v = try allocator.dupe(u8, kvIt.next().?);
                try record.put(k, v);
            }
        } else if (at_record_end) {
            var valid_easy = true;
            var valid_hard = true;
            check(&record, &valid_easy, &valid_hard);
            if (valid_easy) {
                easy_result += 1;
            }
            if (valid_hard) {
                hard_result += 1;
            }

            var it = record.iterator();
            while (it.next()) |kv| {
                allocator.free(kv.key);
                allocator.free(kv.value);
            }
            record.clearRetainingCapacity();
        }
    }

    std.debug.print("EASY: {}\n", .{easy_result});
    std.debug.print("HARD: {}\n", .{hard_result});
}
