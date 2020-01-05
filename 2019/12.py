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
            pos=[int(g) for g in MOON_RE.search(line).groups()],
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


def main():
    data = parse_input(Path('12.txt').read_text())
    print(solve_easy(data))


if __name__ == '__main__':
    main()
