from pathlib import Path


def parse_input(input):
    return [
        [1 if ch == '#' else 0 for ch in line.strip()]
        for line in input.strip().split('\n')
    ]


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
                for dr, dc in ((1, 0), (-1, 0), (0, 1), (0, -1))
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


def test_sample():
    sample = parse_input('''
    ....#
    #..#.
    #..##
    ..#..
    #....
    ''')
    print(solve_easy(sample))


if __name__ == '__main__':
    data = parse_input(Path('24.txt').read_text())
    test_sample()
    print(solve_easy(data))