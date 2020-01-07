from dataclasses import dataclass
from pathlib import Path


def parse_input(input):
    return [
        [1 if ch == '#' else 0 for ch in line.strip()]
        for line in input.strip().split('\n')
    ]


ADJ = ((1, 0), (-1, 0), (0, 1), (0, -1))


def make_step(data):
    def get_new_state(old_state, adj_count):
        if old_state == 0:
            return 1 if adj_count in (1, 2) else 0
        return 1 if adj_count == 1 else 0

    n = len(data)
    return [
        [
            get_new_state(data[row][col], sum(
                data[new_row][new_col]
                for dr, dc in ADJ
                if 0 <= (new_row := row + dr) < n and 0 <= (new_col := col + dc) < n
            ))
            for col in range(n)
        ]
        for row in range(n)
    ]


def find_repeating_layout(data):
    states = set()
    while True:
        if (layout_state := tuple(sum(data, []))) in states:
            return data
        states.add(layout_state)
        data = make_step(data)


def solve_easy(data):
    layout = find_repeating_layout(data)
    n = len(layout)
    return sum(elem * (2 ** idx) for idx, elem in zip(range(n * n), sum(layout, [])))


@dataclass(frozen=True)
class Point:
    level: int
    pos: complex

    def adjacent(self):
        result = set()
        for adj in (complex(*deltas) for deltas in ADJ):
            next_pos, next_pos_inv = self.pos + adj, self.pos - adj
            if next_pos == 0:
                result.update(
                    Point(level=self.level + 1, pos=next_pos_inv + adj * 1j * d)
                    for d in range(-2, 3)
                )
            elif abs(next_pos.real) > 2 or abs(next_pos.imag) > 2:
                result.add(Point(level=self.level - 1, pos=0 + adj))
            else:
                result.add(Point(level=self.level, pos=next_pos))
        return result


def evolve_fractal(data, steps):
    n = len(data)
    state = set(Point(level=0, pos=complex(x - 2, y - 2)) for y in range(n) for x in range(n) if data[y][x])

    def live_adjacent_count(p):
        return len([pp for pp in p.adjacent() if pp in state])

    for step in range(steps):
        new_state = set()
        for p in state:
            if live_adjacent_count(p) == 1:
                new_state.add(p)
            for pp in p.adjacent():
                if pp not in state and live_adjacent_count(pp) in (1, 2):
                    new_state.add(pp)
        state = new_state

    return state


def solve_hard(data):
    return len(evolve_fractal(data, 200))


def test_sample():
    sample = parse_input('''
    ....#
    #..#.
    #..##
    ..#..
    #....
    ''')
    print(solve_easy(sample))
    print(len(evolve_fractal(sample, 10)))

    points = [
        Point(level=0, pos=complex(1, 1)),
        Point(level=1, pos=complex(-1, -1)),
        Point(level=1, pos=complex(1, -2)),
        Point(level=1, pos=complex(2, -2)),
        Point(level=0, pos=complex(1, 0)),
        Point(level=1, pos=complex(1, 0)),
    ]
    for p in points:
        print(', '.join(map(str, p.adjacent())))


if __name__ == '__main__':
    data = parse_input(Path('24.txt').read_text())
    print(solve_easy(data))
    print(solve_hard(data))
