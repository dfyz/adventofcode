from pathlib import Path

from intcode import run_program


def test_sample():
    programs = (
        [3, 0, 4, 0, 99],
        [1101, 100, -1, 4, 0],
    )
    for prog in programs:
        run_program(prog, [1])


def solve_easy(data):
    run_program(data, [1])


def main():
    data = [int(x) for x in Path('05.txt').read_text().split(',')]
    solve_easy(data)


if __name__ == '__main__':
    main()