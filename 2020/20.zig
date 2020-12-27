const std = @import("std");

const Side = enum(u2) {
    top,
    right,
    bottom,
    left,
};

const PlacedTile = struct {
    sides: [4]u64,

    fn init(s0: u64, s1: u64, s2: u64, s3: u64) PlacedTile {
        return PlacedTile{ .sides = [_]u64{ s0, s1, s2, s3 } };
    }

    fn rotate(self: *const PlacedTile) PlacedTile {
        return PlacedTile.init(
            self.sides[3],
            self.sides[0],
            self.sides[1],
            self.sides[2],
        );
    }

    fn flip(self: *const PlacedTile) PlacedTile {
        return PlacedTile.init(
            rev(self.sides[2]),
            rev(self.sides[1]),
            rev(self.sides[0]),
            rev(self.sides[3]),
        );
    }
};

const Variants = [8]PlacedTile;

const Tile = struct {
    id: u64,
    variants: Variants,
};

const TileSize = 10;
const CroppedTileSize = TileSize - 2;
const One = '#';

fn rowToNum(row: []const u8) u64 {
    var result: u64 = 0;
    for (row) |ch, idx| {
        result <<= 1;
        if (ch == One) {
            result |= 1;
        }
    }
    return result;
}

fn rev(side: u64) u64 {
    return @bitReverse(u64, side) >> (64 - TileSize);
}

fn can_be_placed(field: []?*const PlacedTile, cand: *const PlacedTile, row: usize, col: usize, dim: usize) bool {
    const i_row = @intCast(i64, row);
    const i_col = @intCast(i64, col);
    const dr = [_]i64{ -1, 0, 1, 0 };
    const dc = [_]i64{ 0, 1, 0, -1 };
    const our_side = [_]Side{ .top, .right, .bottom, .left };
    const their_side = [_]Side{ .bottom, .left, .top, .right };
    var idx: usize = 0;
    while (idx < dr.len) : (idx += 1) {
        const next_row = i_row + dr[idx];
        const next_col = i_col + dc[idx];
        if (next_row >= 0 and next_col >= 0 and next_row < dim and next_col < dim) {
            const u_next_row = @intCast(u64, next_row);
            const u_next_col = @intCast(u64, next_col);
            if (field[u_next_row * dim + u_next_col]) |neighbor| {
                if (rev(neighbor.sides[@enumToInt(their_side[idx])]) != cand.sides[@enumToInt(our_side[idx])]) {
                    return false;
                }
            }
        }
    }
    return true;
}

fn solve(field: []?*const PlacedTile, ids: []u64, variants: []usize, row: usize, col: usize, dim: usize, free_tiles: []bool, tiles: []const Tile) bool {
    if (row >= dim) {
        return true;
    }

    var next_col = col + 1;
    var next_row = row;
    if (next_col >= dim) {
        next_col = 0;
        next_row += 1;
    }

    const field_idx = row * dim + col;
    for (free_tiles) |free, tile_idx| {
        if (!free) {
            continue;
        }

        free_tiles[tile_idx] = false;
        defer free_tiles[tile_idx] = true;
        const tile = &tiles[tile_idx];

        for (tile.variants) |*v, v_idx| {
            if (can_be_placed(field, v, row, col, dim)) {
                field[field_idx] = v;
                ids[field_idx] = tile.id;
                variants[field_idx] = v_idx;
                if (solve(field, ids, variants, next_row, next_col, dim, free_tiles, tiles)) {
                    return true;
                }
                field[field_idx] = null;
                ids[field_idx] = std.math.maxInt(u64);
                variants[field_idx] = std.math.maxInt(usize);
            }
        }
    }

    return false;
}

const ImageSlice = struct {
    image: []u8,
    rows: usize,
    cols: usize,
    stride: usize,

    fn rotate(self: *ImageSlice) void {
        var row: usize = 0;
        while (row < self.rows) : (row += 1) {
            var col: usize = row + 1;
            while (col < self.cols) : (col += 1) {
                self.swap(row, col, col, row);
            }
        }

        row = 0;
        while (row < self.rows) : (row += 1) {
            var col: usize = 0;
            while (col * 2 < self.cols) : (col += 1) {
                self.swap(row, col, row, self.cols - 1 - col);
            }
        }
    }

    fn flip(self: *ImageSlice) void {
        var row: usize = 0;
        while (row * 2 < self.rows) : (row += 1) {
            var col: usize = 0;
            while (col < self.cols) : (col += 1) {
                self.swap(row, col, self.rows - 1 - row, col);
            }
        }
    }

    fn get_roughness(self: *const ImageSlice, monster: *const ImageSlice) ?u64 {
        var monster_count: u64 = 0;
        var row: usize = 0;
        var sharp_count: u64 = 0;
        while (row < self.rows) : (row += 1) {
            var col: usize = 0;
            while (col < self.cols) : (col += 1) {
                if (self.get(row, col) == One) {
                    sharp_count += 1;
                }
                if (self.has_sea_monster(row, col, monster)) {
                    monster_count += 1;
                }
            }
        }
        return if (monster_count > 0) sharp_count - (monster_count * std.mem.count(u8, monster.image, "#")) else null;
    }

    fn has_sea_monster(self: *const ImageSlice, r: usize, c: usize, monster: *const ImageSlice) bool {
        if (r + monster.rows > self.rows or c + monster.cols > self.cols) {
            return false;
        }
        var dr: usize = 0;
        while (dr < monster.rows) : (dr += 1) {
            var dc: usize = 0;
            while (dc < monster.cols) : (dc += 1) {
                const sc = monster.get(dr, dc);
                if (sc == One and self.get(r + dr, c + dc) != sc) {
                    return false;
                }
            }
        }
        return true;
    }

    fn get(self: *const ImageSlice, r: usize, c: usize) u8 {
        return self.image[self.to_idx(r, c)];
    }

    fn set(self: *ImageSlice, r: usize, c: usize, val: u8) void {
        self.image[self.to_idx(r, c)] = val;
    }

    fn swap(self: *ImageSlice, r1: usize, c1: usize, r2: usize, c2: usize) void {
        const tmp = self.get(r2, c2);
        self.set(r2, c2, self.get(r1, c1));
        self.set(r1, c1, tmp);
    }

    fn to_idx(self: *const ImageSlice, r: usize, c: usize) usize {
        return r * self.stride + c;
    }
};

fn extract_id(title: []const u8) u64 {
    return std.fmt.parseInt(u64, title[5..9], 10) catch unreachable;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var allocator = &arena.allocator;

    var input = try std.fs.cwd().readFileAlloc(allocator, "20.txt", std.math.maxInt(usize));
    var it = std.mem.tokenize(input, "\n");

    var tiles = std.ArrayList(Tile).init(allocator);
    while (it.next()) |title| {
        const id = extract_id(title);
        var idx: usize = 0;
        var sides = [1]u64{0} ** 4;
        var left: u64 = 0;
        var right: u64 = 0;
        while (idx < TileSize) : (idx += 1) {
            const row = it.next().?;
            if (idx == 0) {
                sides[@enumToInt(Side.top)] = rev(rowToNum(row));
            } else if (idx + 1 == TileSize) {
                sides[@enumToInt(Side.bottom)] = rowToNum(row);
            }

            left <<= 1;
            right <<= 1;
            if (row[0] == One) {
                left |= 1;
            }
            if (row[row.len - 1] == One) {
                right |= 1;
            }
        }
        sides[@enumToInt(Side.left)] = left;
        sides[@enumToInt(Side.right)] = rev(right);

        var tile = Tile{
            .id = id,
            .variants = std.mem.zeroes(Variants),
        };
        var placed = PlacedTile{ .sides = sides };
        var rot_idx: usize = 0;
        while (rot_idx < tile.variants.len) : (rot_idx += 2) {
            tile.variants[rot_idx] = placed;
            tile.variants[rot_idx + 1] = placed.flip();
            placed = placed.rotate();
        }
        try tiles.append(tile);
    }

    const size = tiles.items.len;
    const dim = std.math.sqrt(size);

    var field = try allocator.alloc(?*const PlacedTile, size);
    std.mem.set(?*const PlacedTile, field, null);

    var ids = try allocator.alloc(u64, size);
    std.mem.set(u64, ids, std.math.maxInt(u64));

    var variants = try allocator.alloc(usize, size);
    std.mem.set(usize, variants, std.math.maxInt(usize));

    var free_tiles = try allocator.alloc(bool, size);
    std.mem.set(bool, free_tiles, true);

    const solved = solve(field, ids, variants, 0, 0, dim, free_tiles, tiles.items);
    std.debug.assert(solved);

    const easy_answer = ids[0] * ids[dim - 1] * ids[size - dim] * ids[size - 1];
    std.debug.print("EASY: {}\n", .{easy_answer});

    var image = try allocator.alloc(u8, size * CroppedTileSize * CroppedTileSize);

    it = std.mem.tokenize(input, "\n");
    while (it.next()) |line| {
        const id = extract_id(line);
        const pos = std.mem.indexOfScalar(u64, ids, id).?;

        _ = it.next().?;

        var row_idx: usize = 0;
        const start_idx = (pos / dim) * CroppedTileSize * CroppedTileSize * dim + (pos % dim) * CroppedTileSize;
        var slice = ImageSlice{
            .image = image[start_idx..],
            .rows = CroppedTileSize,
            .cols = CroppedTileSize,
            .stride = CroppedTileSize * dim,
        };
        while (row_idx < CroppedTileSize) : (row_idx += 1) {
            const mini_row = (it.next().?)[1 .. TileSize - 1];
            for (mini_row) |val, col_idx| {
                slice.set(row_idx, col_idx, val);
            }
        }

        _ = it.next().?;

        const variant = variants[pos];
        const rot_count = variant / 2;
        const should_flip = variant % 2 == 1;
        var rot_idx: usize = 0;
        while (rot_idx < rot_count) : (rot_idx += 1) {
            slice.rotate();
        }
        if (should_flip) {
            slice.flip();
        }
    }

    const sea_monster_content = "                  # #    ##    ##    ### #  #  #  #  #  #   ";
    var sea_monster_buf = try allocator.alloc(u8, sea_monster_content.len);
    std.mem.copy(u8, sea_monster_buf, sea_monster_content);
    const sea_monster = ImageSlice{
        .image = sea_monster_buf,
        .rows = 3,
        .cols = 20,
        .stride = 20,
    };

    var mega_image = ImageSlice{
        .image = image,
        .rows = dim * CroppedTileSize,
        .cols = dim * CroppedTileSize,
        .stride = dim * CroppedTileSize,
    };

    const hard_answer = blk: {
        var rot_idx: usize = 0;
        while (rot_idx < 4) : (rot_idx += 1) {
            if (mega_image.get_roughness(&sea_monster)) |r| {
                break :blk r;
            }
            mega_image.flip();
            if (mega_image.get_roughness(&sea_monster)) |r| {
                break :blk r;
            }
            mega_image.flip();
            mega_image.rotate();
        }
        unreachable;
    };

    std.debug.print("HARD: {}\n", .{hard_answer});
}
