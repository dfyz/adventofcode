import collections
import functools
import itertools
import math
from pathlib import Path


def parse_map(map):
    data = [stripped for x in map.split('\n') if (stripped := x.strip())]
    return data, len(data), len(data[0])


def get_coords(rows, cols):
    return itertools.product(range(rows), range(cols))


def get_asteroids(data, rows, cols):
    return set((r, c) for r, c in get_coords(rows, cols) if data[r][c] == '#')


def find_best_place(data, rows, cols):
    asteroids = get_asteroids(data, rows, cols)
    assert asteroids

    result = {}
    for r1, c1 in asteroids:
        visible_asteroids = asteroids.copy() - {(r1, c1)}
        for r2, c2 in get_coords(rows, cols):
            delta_row = r2 - r1
            delta_col = c2 - c1
            if math.gcd(abs(delta_row), abs(delta_col)) != 1:
                continue
            blocked = False
            while (0 <= r2 < rows) and (0 <= c2 < cols):
                if (r2, c2) in asteroids:
                    if blocked:
                        visible_asteroids.remove((r2, c2))
                    blocked = True
                r2 += delta_row
                c2 += delta_col
        result[(r1, c1)] = len(visible_asteroids)

    return max(result.items(), key=lambda p: p[1])


def solve_easy(map):
    return find_best_place(*parse_map(map))[1]


def cross(x1, y1, x2, y2):
    return x1 * y2 - y1 * x2


def get_quarter(x, y):
    assert not x == y == 0
    if x >= 0:
        return 0 if y > 0 else (2 if x == 0 else 1)
    return 2 if y < 0 else 3


def compare_vectors_by_angle(v1, v2):
    x1, y1 = v1
    x2, y2 = v2
    q1, q2 = get_quarter(x1, y1), get_quarter(x2, y2)
    if q1 != q2:
        return q1 - q2
    return cross(x1, y1, x2, y2)


def solve_hard(map):
    data, rows, cols = parse_map(map)
    place, _ = find_best_place(data, rows, cols)
    asteroids = get_asteroids(data, rows, cols) - {place}

    vectors = collections.defaultdict(list)
    place_r, place_c = place
    for r, c in asteroids:
        x, y = c - place_c, place_r - r
        g = math.gcd(x, y)
        vectors[(x // g, y // g)].append(((x, y), (c, r)))
    for vals in vectors.values():
        vals.sort(key=lambda p: p[0][0]**2 + p[0][1]**2)
    sorted_keys = sorted(vectors, key=functools.cmp_to_key(compare_vectors_by_angle))
    vaporized = 0
    for key in itertools.cycle(sorted_keys):
        if vals := vectors[key]:
            vaporized += 1
            if vaporized == 200:
                res_x, res_y = vals[0][1]
                return res_x * 100 + res_y
            vals.pop(0)


def test_sample():
    maps = [
        '''
        .#..#
        .....
        #####
        ....#
        ...##
        ''',
        '''
        ......#.#.
        #..#.#....
        ..#######.
        .#.#.###..
        .#..#.....
        ..#....#.#
        #..#....#.
        .##.#..###
        ##...#..#.
        .#....####
        ''',
        '''
        #.#...#.#.
        .###....#.
        .#....#...
        ##.#.#.#.#
        ....#.#.#.
        .##..###.#
        ..#...##..
        ..##....##
        ......#...
        .####.###.
        ''',
        '''
        .#..#..###
        ####.###.#
        ....###.#.
        ..###.##.#
        ##.##.#.#.
        ....###..#
        ..#.#..#.#
        #..#.#.###
        .##...##.#
        .....#.#..
        ''',
        '''
        .#..##.###...#######
        ##.############..##.
        .#.######.########.#
        .###.#######.####.#.
        #####.##.#.##.###.##
        ..#####..#.#########
        ####################
        #.####....###.#.#.##
        ##.#################
        #####.##.###..####..
        ..######..##.#######
        ####.##.####...##..#
        .#####..#.######.###
        ##...#.##########...
        #.##########.#######
        .####.#.###.###.#.##
        ....##.##.###..#####
        .#.#.###########.###
        #.#.#.#####.####.###
        ###.##.####.##.#..##
        '''
    ]
    for m in maps:
        print(solve_easy(m))
    print(solve_hard(maps[-1]))


def main():
    map = Path('10.txt').read_text()
    print(solve_easy(map))
    print(solve_hard(map))


if __name__ == '__main__':
    main()
