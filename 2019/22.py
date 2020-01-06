import collections
from pathlib import Path


def parse_input(input):
    def parse_line(line):
        command, *args, last_arg = line.strip().split()
        if last_arg == 'stack':
            return 'reverse', None
        return command, int(last_arg)

    return list(map(parse_line, input.strip().split('\n')))


def naive_shuffle(commands, n):
    deck = collections.deque(range(n))
    for cmd, arg in commands:
        if cmd == 'reverse':
            deck.reverse()
        elif cmd == 'cut':
            deck.rotate(-arg)
        elif cmd == 'deal':
            next_deck = [(idx * arg % n, elem) for idx, elem in enumerate(deck)]
            next_deck.sort(key=lambda p: p[0])
            deck = collections.deque(p[1] for p in next_deck)
    return deck


def mat_mul(m1, m2, n):
    dim = len(m1)
    return [
        [
            sum(m1[row][k] * m2[k][col] for k in range(dim)) % n
            for col in range(dim)
        ]
        for row in range(dim)
    ]


def mat_pow(mat, p, n):
    if p == 0:
        return [[1, 0], [0, 1]]
    result = mat_pow(mat, p // 2, n)
    result = mat_mul(result, result, n)
    if p % 2 == 1:
        result = mat_mul(result, mat, n)
    return result


def shuffle(commands, n, p):
    def transform(cmd, arg):
        if cmd == 'reverse':
            return -1, -1
        elif cmd == 'cut':
            return 1, -arg
        elif cmd == 'deal':
            return arg, 0

    a, b = 1, 0
    for full_cmd in commands:
        next_a, next_b = map(lambda x: x % n, transform(*full_cmd))
        a = (next_a * a) % n
        b = (next_a * b + next_b) % n

    result = mat_pow([[a, b], [0, 1]], p, n)
    a, b = result[0]
    return ((2020 + (-b % n)) * pow(a, n - 2, n)) % n


def solve_easy(commands):
    deck = naive_shuffle(commands, 10007)
    return deck.index(2019)


def solve_hard(commands):
    n = 119315717514047
    p = 101741582076661
    return shuffle(commands, n, p)


def test_sample():
    samples = list(map(parse_input, [
        '''
        deal with increment 7
        deal into new stack
        deal into new stack
        ''',
        '''
        cut 6
        deal with increment 7
        deal into new stack
        ''',
        '''
        deal with increment 7
        deal with increment 9
        cut -2
        ''',
        '''
        deal into new stack
        cut -2
        deal with increment 7
        cut 8
        cut -4
        deal with increment 7
        cut 3
        deal with increment 9
        deal with increment 3
        cut -1
        '''
    ]))
    for s in samples:
        print(naive_shuffle(s, 10))
        print(shuffle(s, 10, 1))


def main():
    data = parse_input(Path('22.txt').read_text())
    print(solve_easy(data))
    print(solve_hard(data))


if __name__ == '__main__':
    main()
