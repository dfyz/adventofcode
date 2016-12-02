import sys
import re

N = 1000
lights = [[0 for i in xrange(N)] for j in xrange(N)]

def parse_coords(coords):
	return map(int, coords.split(','))

for line in sys.stdin:
	line = line.strip()
	action, a, b = re.search('([a-z ]*) (\d+,\d+) through (\d+,\d+)', line).groups()
	x1, y1 = parse_coords(a)
	x2, y2 = parse_coords(b)

	assert x1 <= x2 and y1 <= y2
	switcher = {
		'turn on': lambda x: 1,
		'turn off': lambda x: 0,
		'toggle': lambda x: 1 - x,
	}[action]

	for x in xrange(x1, x2 + 1):
		for y in xrange(y1, y2 + 1):
			lights[x][y] = switcher(lights[x][y])
print sum(sum(row) for row in lights)