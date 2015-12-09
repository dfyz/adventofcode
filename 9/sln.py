import itertools
import sys

dist = {}
cities = set()

for line in sys.stdin:
    a, _, b, __, d = line.strip().split()
    d = int(d)
    dist[a, b] = d
    dist[b, a] = d
    cities.update([a, b])

def consecutive_pairs(seq):
    return [(seq[i], seq[i + 1]) for i in xrange(len(seq) - 1)]

def aggregate_permutations(agg):
    return agg(sum(dist[pair] for pair in consecutive_pairs(pi)) for pi in itertools.permutations(cities))

print aggregate_permutations(min)
print aggregate_permutations(max)
