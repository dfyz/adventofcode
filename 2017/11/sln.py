from __future__ import print_function


DELTAS = {
    'n': (0, 2),
    's': (0, -2),
    'nw': (-2, 1),
    'sw': (-2, -1),
    'ne': (2, 1),
    'se': (2, -1),
}


def get_dist(x, y):
    x_dist = abs(x)
    y_dist = max(0, abs(y) - x_dist / 2)
    return (x_dist + y_dist) / 2


def solve(line):
    tokens = line.split(',')
    x, y = 0, 0
    max_dist = 0
    for d in tokens:
        dx, dy = DELTAS[d]
        x += dx
        y += dy
        max_dist = max(max_dist, get_dist(x, y))
    print(get_dist(x, y))
    print(max_dist)


if __name__ == '__main__':
    with open('input.txt') as inp:
        for line in inp:
            solve(line.rstrip())
