import math
from dataclasses import dataclass
from pathlib import Path
import re
from typing import List

MOON_RE = re.compile(r'''^<x=(.*), y=(.*), z=(.*)>$''')
DIMS = 3


@dataclass
class Moon:
    pos: List[int]
    vel: List[int]


def parse_input(input):
    return [
        Moon(
            pos=[int(g) for g in MOON_RE.search(line.strip()).groups()],
            vel=[0] * DIMS
        )
        for line in input.strip().split('\n')
    ]


def move_moons(moons):
    def get_delta(p1, p2):
        if p1 < p2:
            return 1
        elif p1 > p2:
            return -1
        return 0

    for idx1 in range(len(moons)):
        for idx2 in range(idx1 + 1, len(moons)):
            m1, m2 = moons[idx1], moons[idx2]
            for d, (p1, p2) in enumerate(zip(m1.pos, m2.pos)):
                delta = get_delta(p1, p2)
                m1.vel[d] += delta
                m2.vel[d] -= delta

    for m in moons:
        for d in range(DIMS):
            m.pos[d] += m.vel[d]


def solve_easy(moons):
    for _ in range(1000):
        move_moons(moons)

    def sum_abs(nums):
        return sum(map(abs, nums))

    return sum([sum_abs(m.pos) * sum_abs(m.vel) for m in moons])


def solve_hard(moons):
    seen_states = [set() for _ in range(DIMS)]
    periods: List = [None] * DIMS
    steps = 0

    while True:
        current_states = [
            tuple((m.pos[d], m.vel[d]) for m in moons)
            for d in range(DIMS)
        ]
        for d in range(DIMS):
            if periods[d] is None and current_states[d] in seen_states[d]:
                periods[d] = steps
            seen_states[d].add(current_states[d])
        if None not in periods:
            break
        move_moons(moons)
        steps += 1

    def lcm(a, b):
        g = math.gcd(a, b)
        return g * (a // g) * (b // g)

    return lcm(periods[0], lcm(periods[1], periods[2]))


def test_sample():
    samples = list(map(parse_input, [
        '''
        <x=-1, y=0, z=2>
        <x=2, y=-10, z=-7>
        <x=4, y=-8, z=8>
        <x=3, y=5, z=-1>
        ''',
        '''
        <x=-8, y=-10, z=0>
        <x=5, y=5, z=10>
        <x=2, y=-7, z=3>
        <x=9, y=-8, z=-3>
        ''',
    ]))

    for moons in samples:
        print(solve_hard(moons))


def main():
    data = parse_input(Path('12.txt').read_text())
    test_sample()
    print(solve_easy(data))
    print(solve_hard(data))


if __name__ == '__main__':
    main()
