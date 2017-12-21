from __future__ import print_function

import math


def parse_particle(line):
    tokens = line.split(', ')
    subtokens = [
        map(int, t[t.find('<') + 1:t.find('>')].split(','))
        for t in tokens
    ]
    return subtokens


def norm(v):
    return math.sqrt(sum([c**2 for c in v]))


def main(inp):
    min_a = None
    min_idx = None
    data = []
    for idx, line in enumerate(inp):
        particle = parse_particle(line.rstrip())
        v, a = norm(particle[1]), norm(particle[2])
        data.append((a, v, idx))
    print(sorted(data)[0][-1])


if __name__ == '__main__':
    import sys
    main(sys.stdin)