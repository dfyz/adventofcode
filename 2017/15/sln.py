from __future__ import print_function

import itertools


def rng(start, factor, mod):
    yield start
    while True:
        start = (start * factor) % 2147483647
        if mod is None or start & (mod - 1) == 0:
            yield start & 0xFFFF


def parse_line(line):
    return int(line.rstrip().split()[-1])


def get_answer(a, b, mod_a, mod_b, cnt):
    return len(list(
        itertools.ifilter(
            lambda p: p[0] == p[1],
            itertools.islice(
                itertools.izip(rng(a, 16807, mod_a), rng(b, 48271, mod_b)),
                cnt
            )
        )
    ))


def main(inp):
    a, b = map(parse_line, inp)
    print(get_answer(a, b, None, None, 40000000))
    print(get_answer(a, b, 4, 8, 5000000))


if __name__ == '__main__':
    import sys
    main(sys.stdin)