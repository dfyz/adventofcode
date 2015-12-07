grid = [
	['1', '-', '8', '*'],
	['*', '11', '*', '4'],
	['18', '-', '4', '+'],
	['*', '9', '-', '22'],
]

import operator
import random

delta_x = [-1, 0, 1, 0]
delta_y = [0, -1, 0, 1]
directions = [
	'east',
	'north',
	'west',
	'south',
]

ops = {
	'+': operator.add,
	'-': operator.sub,
	'*': operator.mul,
}

def random_walk():
	x, y = 3, 3
	current = 0
	op = operator.add
	expr = ''
	moves = []
	while x or y:
		ch = grid[y][x]
		expr += ch
		if ch in '+-*':
			op = ops[ch]
		else:
			current = op(current, int(ch))
			if current <= 0:
				return 0, expr, moves

		while True:
			idx = random.choice(xrange(4))
			new_x, new_y = x + delta_x[idx], y + delta_y[idx]
			if new_x >= 0 and new_y >= 0 and new_x < 4 and new_y < 4 and not (new_x == 3 and new_y == 3):
				x, y = new_x, new_y
				moves.append(directions[idx])
				break

	assert x == 0 and y == 0
	current = op(current, int(grid[0][0]))
	expr += grid[0][0]
	return current, expr, moves

while True:
	res, expr, moves = random_walk()
	if res == 30 and len(moves) < 14:
		print len(moves)
		print expr
		for m in moves:
			print 'go ' + m
		break