import copy

START = ['.#.', '..#', '###']
EASY_ITERS = 5
HARD_ITERS = 18


def rotate(block):
    result = []
    for row_idx in range(len(block)):
        result.append(''.join([
            block[col_idx][row_idx]
            for col_idx in reversed(range(len(block)))
        ]))
    return result


def transform(block):
    for _ in range(4):
        yield block
        yield [''.join(reversed(row)) for row in block]
        block = rotate(block)


def split(state, square_size):
    def carve(start_row, start_col):
        return [
            state[row_idx][start_col:start_col + square_size]
            for row_idx in range(start_row, start_row + square_size)
        ]

    indexes = range(0, len(state), square_size)
    result = []
    for row_idx in indexes:
        result.append([
            carve(row_idx, col_idx) for col_idx in indexes
        ])
    return result


def join(splitted):
    indexes = range(len(splitted))
    result = []
    for row_idx in indexes:
        for sub_row_idx in range(len(splitted[row_idx][0])):
            result.append(''.join([
                splitted[row_idx][col_idx][sub_row_idx]
                for col_idx in indexes
            ]))
    return result


def enhance(patterns, block):
    matching_patterns = [p for p in patterns if p[0] == block]
    assert len({''.join(p[1]) for p in matching_patterns}) == 1, block
    return copy.deepcopy(matching_patterns[0][1])


def on_count(state):
    return sum(row.count('#') for row in state)


def main(inp):
    patterns = []
    for line in inp:
        lhs, rhs = [s.split('/') for s in line.rstrip().split(' => ')]
        for tr in transform(lhs):
            patterns.append((tr, rhs))

    state = START

    for it in range(HARD_ITERS):
        splitted = split(state, 2 + len(state) % 2)

        enhanced = []
        for row in splitted:
            enhanced.append([
                enhance(patterns, block) for block in row
            ])

        state = join(enhanced)
        if it + 1 == EASY_ITERS:
            print(on_count(state))
    print(on_count(state))

if __name__ == '__main__':
    import sys
    main(sys.stdin)
