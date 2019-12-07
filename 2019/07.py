from pathlib import Path
import itertools

from intcode import Program


def find_thruster_output_easy(phase_order, data):
    input_value = 0
    for phase in phase_order:
        p = Program(data.copy(), [phase, input_value])
        p.run()
        input_value = p.outputs[0]
    return input_value


def solve_easy(data):
    return max(find_thruster_output_easy(order, data) for order in itertools.permutations(range(5)))


def find_thruster_output_hard(phase_order, data):
    input_value = 0
    result = None
    programs = [Program(data.copy(), [phase], pause_on_output=True) for phase in phase_order]
    program_idx = 0
    for phase, program in itertools.cycle(zip(phase_order, programs)):
        program.inputs.append(input_value)
        program.run()
        if program.halted:
            break
        input_value = program.outputs[-1]
        program_idx += 1
        if program_idx >= len(programs):
            result = input_value
            program_idx = 0
    return result


def solve_hard(data):
    return max(find_thruster_output_hard(order, data) for order in itertools.permutations(range(5, 10)))


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
    hard_programs = [
        [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
         27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5],
        [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54,
         -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4,
         53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10],
    ]
    for prog in hard_programs:
        print(solve_hard(prog))


def main():
    data = [int(x) for x in Path('07.txt').read_text().split(',')]
    print(solve_easy(data))
    print(solve_hard(data))


if __name__ == '__main__':
    main()
