from pathlib import Path

from intcode import Program


def eval_program(data, in1, in2):
    data[1], data[2] = in1, in2
    Program(data, []).run()
    return data[0]


def solve_easy(data):
    return eval_program(data, 12, 2)


def solve_hard(data, magic):
    upper_bound = 99 + 1
    for in1 in range(upper_bound):
        for in2 in range(upper_bound):
            eval_result = eval_program(data.copy(), in1, in2)
            if eval_result == magic:
                return in1 * 100 + in2
    else:
        raise Exception('Failed to find a suitable noun/verb')


def test_sample():
    test_programs = [
        [1, 0, 0, 0, 99],
        [2, 3, 0, 3, 99],
        [2, 4, 4, 5, 99, 0],
        [1, 1, 1, 4, 99, 5, 6, 0, 99],
    ]
    for prog in test_programs:
        print('BEFORE', prog)
        Program(prog, []).run()
        print('AFTER', prog)


def main():
    data = [int(x) for x in Path('02.txt').read_text().split(',')]
    magic = int(Path('02_magic.txt').read_text())
    print(solve_easy(data.copy()))
    print(solve_hard(data.copy(), magic))


if __name__ == '__main__':
    main()
