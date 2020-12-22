const std = @import("std");

const StrList = std.StringHashMap(void);

const Food = struct {
    allocator: *std.mem.Allocator,
    ingredients: StrList,
    allergens: StrList,

    fn init(allocator: *std.mem.Allocator) Food {
        return Food{
            .allocator = allocator,
            .ingredients = StrList.init(allocator),
            .allergens = StrList.init(allocator),
        };
    }

    fn deinit(self: *Food) void {
        self.ingredients.deinit();
        self.allergens.deinit();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    const input = try std.fs.cwd().readFileAlloc(allocator, "21.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    var foods = std.ArrayList(Food).init(allocator);
    defer {
        for (foods.items) |*f| {
            f.deinit();
        }
        foods.deinit();
    }

    var line_it = std.mem.tokenize(input, "\n");
    while (line_it.next()) |line| {
        var food = Food.init(allocator);

        var token_it = std.mem.tokenize(line, " (),");
        var allergens_started = false;
        while (token_it.next()) |token| {
            if (std.mem.eql(u8, token, "contains")) {
                allergens_started = true;
            } else {
                if (allergens_started) {
                    try food.allergens.put(token, {});
                } else {
                    try food.ingredients.put(token, {});
                }
            }
        }

        try foods.append(food);
    }

    var good_ingredients = std.StringHashMap(void).init(allocator);
    defer good_ingredients.deinit();

    var all_alergens = std.StringHashMap(void).init(allocator);
    defer all_alergens.deinit();

    for (foods.items) |*food| {
        var lhs_it = food.ingredients.iterator();
        while (lhs_it.next()) |lhs| {
            var found_match = false;

            var rhs_it = food.allergens.iterator();
            while (rhs_it.next()) |rhs| {
                try all_alergens.put(rhs.key, {});
                var all_good = true;

                for (foods.items) |*other_food| {
                    if (other_food.allergens.contains(rhs.key) and !other_food.ingredients.contains(lhs.key)) {
                        all_good = false;
                        break;
                    }
                }
                if (all_good) {
                    found_match = true;
                    break;
                }
            }

            if (found_match) {
                try good_ingredients.put(lhs.key, {});
            }
        }
    }

    var easy_answer: u64 = 0;
    for (foods.items) |*food| {
        var lhs_it = food.ingredients.iterator();
        while (lhs_it.next()) |lhs| {
            easy_answer += @boolToInt(!good_ingredients.contains(lhs.key));
        }
    }
    std.debug.print("EASY: {}\n", .{easy_answer});

    var ingredient_counts = std.StringHashMap(u64).init(allocator);
    defer ingredient_counts.deinit();

    var matches = std.StringHashMap([]const u8).init(allocator);
    defer matches.deinit();

    while (matches.count() < all_alergens.count()) {
        var allergen_it = all_alergens.iterator();
        outer: while (allergen_it.next()) |allergen| {
            ingredient_counts.clearRetainingCapacity();
            var total: u64 = 0;

            for (foods.items) |*food, idx| {
                if (food.allergens.contains(allergen.key)) {
                    total += 1;
                    var ingredient_it = food.ingredients.iterator();
                    while (ingredient_it.next()) |ingredient| {
                        var gop = try ingredient_counts.getOrPut(ingredient.key);
                        if (gop.found_existing) {
                            gop.entry.value += 1;
                        } else {
                            gop.entry.value = 1;
                        }
                    }
                }
            }

            var ingredient_it = ingredient_counts.iterator();
            var possible_ingredient: ?[]const u8 = null;
            while (ingredient_it.next()) |ingredient| {
                var matched = false;
                var match_it = matches.iterator();
                while (match_it.next()) |kv| {
                    if (std.mem.eql(u8, kv.value, ingredient.key)) {
                        matched = true;
                        break;
                    }
                }
                if (!matched and ingredient.value == total) {
                    if (possible_ingredient == null) {
                        possible_ingredient = ingredient.key;
                    } else {
                        continue :outer;
                    }
                }
            }

            if (possible_ingredient) |ingredient| {
                try matches.put(allergen.key, ingredient);
            }
        }
    }

    std.debug.print("HARD: ", .{});

    while (matches.count() > 0) {
        var min_key: ?[]const u8 = null;
        var match_it = matches.iterator();
        while (match_it.next()) |match| {
            if (min_key == null or std.mem.order(u8, match.key, min_key.?) == .lt) {
                min_key = match.key;
            }
        }

        const separator = [_]u8{if (matches.count() == 1) '\n' else ','};
        std.debug.print("{}{}", .{ matches.get(min_key.?).?, separator });
        matches.removeAssertDiscard(min_key.?);
    }
}
