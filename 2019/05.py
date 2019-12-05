from pathlib import Path

from intcode import Program


def test_sample():
    programs = (
        [3, 0, 4, 0, 99],
        [1101, 100, -1, 4, 0],
    )
    for prog in programs:
        p = Program(prog, [1])
        p.run()
        print(p.outputs)


def solve_easy(data):
    p = Program(data, [1])
    p.run()
    return p.outputs[-1]


def main():
    data = [int(x) for x in Path('05.txt').read_text().split(',')]
    print(solve_easy(data))


if __name__ == '__main__':
    main()