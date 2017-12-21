from __future__ import print_function

import copy
import math


DIM = 3
STEPS = 100


def parse_particle(line):
    tokens = line.split(', ')
    subtokens = [
        map(int, t[t.find('<') + 1:t.find('>')].split(','))
        for t in tokens
    ]
    return subtokens


def norm(v):
    return math.sqrt(sum([c**2 for c in v]))


def dist(coords1, coords2):
    return norm([c1 - c2 for c1, c2 in zip(coords1, coords2)])


def collide(ps):
    ps = copy.deepcopy(ps)
    for _ in xrange(STEPS):
        coords1, coords2 = ps[0][0], ps[1][0]
        if coords1 == coords2:
            return True

        for p in ps:
            for c_idx in xrange(DIM):
                for attr_idx in xrange(DIM - 1, 0, -1):
                    p[attr_idx - 1][c_idx] += p[attr_idx][c_idx]

    return False


def main(inp):
    min_a = None
    min_idx = None
    particles = []
    data = []
    for idx, line in enumerate(inp):
        particle = parse_particle(line.rstrip())
        particles.append(particle)
        v, a = norm(particle[1]), norm(particle[2])
        data.append((a, v, idx))
    print(min(data)[-1])

    n = len(particles)
    active = set(range(n))
    for p1 in xrange(n):
        for p2 in xrange(p1 + 1, n):
            if p1 in active or p2 in active:
                if collide([particles[p1], particles[p2]]):
                    active -= {p1, p2}
    print(len(active))


if __name__ == '__main__':
    import sys
    main(sys.stdin)