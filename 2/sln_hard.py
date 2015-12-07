import operator
import sys

result = 0
for line in sys.stdin.readlines():
	sides = map(int, line.strip().split('x'))
	volume = reduce(operator.mul, sides)
	sides *= 2
	perimeters = [2*(sides[i] + sides[i + 1]) for i in xrange(3)]
	smallest_perimeter = min(perimeters)
	result += smallest_perimeter + volume
print result