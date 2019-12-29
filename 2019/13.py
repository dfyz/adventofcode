from pathlib import Path

from intcode import Program


def get_field(data):
    result = {}
    p = Program(data, [], pause_on_output=True)
    while not p.halted:
        p.outputs.clear()
        p.run_times(3)
        if not p.halted:
            x, y, tile = p.outputs
            result[(x, y)] = tile
    return result


def solve_easy(data):
    return sum(1 for x in get_field(data).values() if x == 2)


def main():
    data = [int(x) for x in Path('13.txt').read_text().split(',')]
    print(solve_easy(data))


if __name__ == '__main__':
    main()