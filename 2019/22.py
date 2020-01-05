import collections
from pathlib import Path


def parse_input(input):
    def parse_line(line):
        command, *args, last_arg = line.strip().split()
        if last_arg == 'stack':
            return ('reverse', None)
        return (command, int(last_arg))

    return list(map(parse_line, input.strip().split('\n')))


def shuffle(commands, n):
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


def solve_easy(commands):
    deck = shuffle(commands, 10007)
    return deck.index(2019)


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
        print(shuffle(s, 10))


def main():
    data = parse_input(Path('22.txt').read_text())
    test_sample()
    print(solve_easy(data))


if __name__ == '__main__':
    main()