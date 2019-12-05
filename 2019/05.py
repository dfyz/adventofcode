from pathlib import Path

from intcode import Program


def test_sample():
    programs = (
        [3, 0, 4, 0, 99],
        [1101, 100, -1, 4, 0],
        [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8],
        [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8],
        [3, 3, 1108, -1, 8, 3, 4, 3, 99],
        [3, 3, 1107, -1, 8, 3, 4, 3, 99],
        [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
         1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
         999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99],
    )
    for prog in programs:
        p = Program(prog, [9])
        p.run()
        print(p.outputs)
    exit(0)


def solve_easy(data):
    p = Program(data, [1])
    p.run()
    return p.outputs[-1]


def solve_hard(data):
    p = Program(data, [5])
    p.run()
    return p.outputs[0]


def main():
    data = [int(x) for x in Path('05.txt').read_text().split(',')]
    print(solve_easy(data.copy()))
    print(solve_hard(data.copy()))


if __name__ == '__main__':
    main()
