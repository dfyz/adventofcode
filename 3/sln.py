import sys

msg = sys.stdin.readline().strip()

visited = set([(0, 0)])
x = 0
y = 0
for ch in msg:
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
print len(visited)