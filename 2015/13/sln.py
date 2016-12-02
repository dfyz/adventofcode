import collections
import itertools
import sys

guests = set()
weights = collections.defaultdict(int)

for line in sys.stdin:
	tokens = line.rstrip(".\n").split()
	src, dst = tokens[0], tokens[-1]
	d = int(tokens[3])
	if tokens[2] == 'lose':
		d = -d
	guests.add(src)
	weights[src, dst] = d

def clamp(num, n):
	return (num + n) % n

def w(perm, start, delta):
	n = len(perm)
	return weights[perm[clamp(start, n)], perm[clamp(start + delta, n)]]

def solve(guest_list):
	n = len(guest_list)
	return max(sum(w(p, i, 1) + w(p, i, -1) for i in xrange(n)) for p in itertools.permutations(guest_list))

print solve(guests)
print solve(guests | set(['Me']))
