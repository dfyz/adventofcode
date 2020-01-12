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


class ModBinomial:
    def __init__(self, mod):
        self.mod = mod
        self.a, self.b = 1, 1
        self.a_degree, self.b_degree = 0, 0

    def get_value(self):
        if self.a_degree > self.b_degree:
            return 0
        return (self.a * pow(self.b, self.mod - 2, self.mod)) % self.mod

    def update(self, a_mul, b_mul):
        def remove_mods(num):
            degree = 0
            while num % self.mod == 0:
                num //= self.mod
                degree += 1
            return num, degree

        a_mul, a_degree = remove_mods(a_mul)
        b_mul, b_degree = remove_mods(b_mul)
        self.a_degree += a_degree
        self.b_degree += b_degree
        self.a = (self.a * a_mul) % self.mod
        self.b = (self.b * b_mul) % self.mod


def generate_binomials(steps):
    crt = [[None for _ in range(2)] for _ in range(5)]
    for x in range(10):
        crt[x % 5][x % 2] = x

    mod2 = ModBinomial(2)
    mod5 = ModBinomial(5)
    for x in itertools.count(start=1):
        yield crt[mod5.get_value()][mod2.get_value()]
        for m in (mod2, mod5):
            m.update(steps + x - 1, x)


def extract_digits_after_steps(nums, total_len, steps, digit_start, digit_count):
    assert total_len % 2 == 0
    first_allowed_offset = total_len // 2

    assert digit_start >= first_allowed_offset
    assert digit_start + digit_count <= total_len

    result = [0] * digit_count
    for pos, coef in zip(range(digit_start, total_len), generate_binomials(steps)):
        for digit in range(digit_count):
            if pos + digit < total_len:
                to_add = (coef * nums[(pos + digit) % len(nums)]) % 10
                result[digit] = (result[digit] + to_add) % 10
    return result


def solve_hard(data):
    total_len = len(data) * 10000
    steps = 100
    digit_start = int(''.join(map(str, data[:7])))
    digits = extract_digits_after_steps(data, total_len, steps, digit_start, 8)
    return ''.join(map(str, digits))


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

    hard_samples = list(map(parse_input, [
        '03036732577212944063491565474664',
        '02935109699940807407585447034323',
        '03081770884921959731165446850517',
    ]))
    for s in hard_samples:
        offset = int(''.join(map(str, s[:7])))
        print(extract_digits_after_steps(s, len(s) * 10000, 100, offset, 8))


if __name__ == '__main__':
    data = parse_input(Path('16.txt').read_text())
    print(solve_easy(data))
    print(solve_hard(data))
