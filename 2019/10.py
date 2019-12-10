import itertools
import math
from pathlib import Path


def parse_map(map):
    return [stripped for x in map.split('\n') if (stripped := x.strip())]


def solve_easy(data):
    rows, cols = len(data), len(data[0])
    coords = lambda: itertools.product(range(rows), range(cols))
    asteroids = set((r, c) for r, c in coords() if data[r][c] == '#')
    assert asteroids

    result = {}
    for r1, c1 in asteroids:
        visible_asteroids = asteroids.copy() - {(r1, c1)}
        for r2, c2 in coords():
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

    return max(result.values())


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
        print(solve_easy(parse_map(m)))


def main():
    test_sample()
    data = parse_map(Path('10.txt').read_text())
    print(solve_easy(data))


if __name__ == '__main__':
    main()
