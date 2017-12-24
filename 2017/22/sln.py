import collections

DIRS = [(-1, 0), (0, 1), (1, 0), (0, -1)]

EASY_ACTIONS = {
    'C': ('I', -1),
    'I': ('C', 1),
}

HARD_ACTIONS = {
    'C': ('W', -1),
    'W': ('I', 0),
    'I': ('F', 1),
    'F': ('C', 2),
}


def run_virus(board, actions, iters):
    size = len(board)
    states = collections.defaultdict(lambda: 'C')
    for row_idx in range(size):
        for col_idx in range(size):
            if board[row_idx][col_idx] == '#':
                states[(row_idx, col_idx)] = 'I'

    r = size // 2
    c = r
    d = 0
    result = 0
    for it in range(iters):
        new_state, dir_inc = actions[states[(r, c)]]
        states[(r, c)] = new_state
        if new_state == 'I':
            result += 1
        d = (d + dir_inc + len(DIRS)) % len(DIRS)
        dd = DIRS[d]
        r += dd[0]
        c += dd[1]
    return result


def main(inp):
    board = [line.rstrip() for line in inp]
    print(run_virus(board, EASY_ACTIONS, 10000))
    print(run_virus(board, HARD_ACTIONS, 10000000))


if __name__ == '__main__':
    import sys
    main(sys.stdin)