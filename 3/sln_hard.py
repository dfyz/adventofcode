import sys

msg = sys.stdin.readline().strip()

def visit_cities(program):
	visited = set([(0, 0)])
	x = 0
	y = 0
	for ch in program:
		if ch == '^':
			y -= 1
		elif ch == '<':
			x -= 1
		elif ch == 'v':
			y += 1
		else:
			assert ch == '>'
			x += 1
		visited.add((x, y))
	return visited

santa_visited = visit_cities(msg[::2])
robo_santa_visited = visit_cities(msg[1::2])

total_visited = santa_visited | robo_santa_visited
print len(total_visited)