import sys

result = 0
for line in sys.stdin.readlines():
	sides = map(int, line.strip().split('x'))
	sides *= 2
	areas = [sides[i]*sides[i + 1] for i in xrange(3)]
	slack = min(areas)
	needed = 2 * sum(areas)
	result += slack + needed
print result