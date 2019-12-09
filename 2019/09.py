from pathlib import Path

from intcode import Program


def test_sample():
    programs = [
        [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99],
        [1102, 34915192, 34915192, 7, 4, 7, 99, 0],
        [104, 1125899906842624, 99],
    ]
    for prog in programs:
        print(solve_easy(prog))

def run_boost(data, input_value):
    p = Program(data.copy(), [input_value])
    p.run()
    assert p.halted
    assert len(p.outputs) == 1
    return p.outputs[0]


def solve_easy(data):
    return run_boost(data, 1)


def solve_hard(data):
    return run_boost(data, 2)


def main():
    data = [int(x) for x in Path('09.txt').read_text().split(',')]
    print(solve_easy(data.copy()))
    print(solve_hard(data.copy()))


if __name__ == '__main__':
    main()
