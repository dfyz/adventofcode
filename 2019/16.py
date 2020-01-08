import itertools
from pathlib import Path


def parse_input(input):
    return list(map(int, input.strip()))


def get_matrix(n):
    def base_pattern(row):
        return itertools.cycle(itertools.chain(*(itertools.repeat(x, row) for x in (0, 1, 0, -1))))

    return [list(itertools.islice(base_pattern(row + 1), 1, n + 1)) for row in range(n)]


def make_steps(nums, steps):
    mat = get_matrix(len(nums))
    for _ in range(steps):
        nums = [abs(sum(a * b for a, b in zip(row, nums))) % 10 for row in mat]
    return nums


def solve_easy(nums):
    return ''.join(str(x) for x in make_steps(nums, 100)[:8])


def test_sample():
    samples = list(map(parse_input, [
        '12345678',
        '80871224585914546619083218645595',
        '19617804207202209144916044189917',
        '69317163492948606335995924319873',
    ]))

    print(make_steps(samples[0], 4))
    for s in samples[1:]:
        print(make_steps(s, 100))


if __name__ == '__main__':
    data = parse_input(Path('16.txt').read_text())
    print(solve_easy(data))
