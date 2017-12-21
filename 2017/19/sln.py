from __future__ import print_function

import string


DIRS = [(1, 0), (0, 1), (-1, 0), (0, -1)]


def main(inp):
    board = [line.rstrip('\n') for line in inp]
    row = 0
    col = board[0].find('|')
    d = 0

    def is_good_cell(r, c):
        return 0 <= r < len(board) and 0 <= c < len(board[r]) and board[r][c] != ' '

    def advance(r, c, d):
        dr, dc = DIRS[d % len(DIRS)]
        return r + dr, c + dc

    def is_empty(r, c):
        return board[r][c] == ' '

    easy_answer = ''
    hard_answer = 0

    while is_good_cell(row, col):
        ch = board[row][col]
        if ch == '+':
            for next_d in (d + 1, d + 3):
                next_row, next_col = advance(row, col, next_d)
                if is_good_cell(next_row, next_col):
                    row, col, d = next_row, next_col, next_d
                    hard_answer += 1
                    break
        else:
            if ch in string.ascii_letters:
                easy_answer += ch
            row, col = advance(row, col, d)
            hard_answer += 1

    print(easy_answer, hard_answer)


if __name__ == '__main__':
    import sys
    main(sys.stdin)