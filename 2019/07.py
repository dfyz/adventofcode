from pathlib import Path
import itertools

from intcode import Program


def find_thruster_output(phase_order, data):
    input_value = 0
    for phase in phase_order:
        p = Program(data.copy(), [phase, input_value])
        p.run()
        input_value = p.outputs[0]
    return input_value


def solve_easy(data):
    return max(find_thruster_output(order, data) for order in itertools.permutations(range(5)))


def test_sample():
    programs = [
        [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0],
        [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
         101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0],
        [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
         1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0],
    ]
    for prog in programs:
        print(solve_easy(prog))


def main():
    data = [int(x) for x in Path('07.txt').read_text().split(',')]
    print(solve_easy(data))


if __name__ == '__main__':
    main()
