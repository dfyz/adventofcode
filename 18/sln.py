import copy
import sys

board = [line.rstrip() for line in sys.stdin.readlines()]
n, m = len(board), len(board[0])
steps = int(sys.argv[1])
is_hard = sys.argv[2] == 'hard'

def is_stuck(row, col, board):
	return is_hard and (row, col) in [(0, 0), (0, m - 1), (n - 1, 0), (n - 1, m - 1)]

def is_on(row, col, board):
	if row < 0 or col < 0 or row >= n or col >= m:
		return False
	if is_stuck(row, col, board):
		return True
	return board[row][col] == '#'

def neighbors(row, col):
	deltas = lambda: xrange(-1, 2)
	return [(row + dr, col + dc) for dr in deltas() for dc in deltas() if (dr, dc) != (0, 0)]

def get_symbol(is_alive):
	return '.#'[is_alive]

def lives(row, col, board):
	if is_stuck(row, col, board):
		return True
	alive_neighbor_count = [is_on(row1, col1, board) for row1, col1 in neighbors(row, col)].count(True)
	is_alive_now = is_on(row, col, board)
	return alive_neighbor_count == 3 or (alive_neighbor_count == 2 and is_alive_now)

def do_step(board):
	return [[get_symbol(lives(row, col, board)) for col in xrange(m)] for row in xrange(n)]

for i in xrange(steps):
	board = do_step(board)

print sum([row.count('#') for row in board])