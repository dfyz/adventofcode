from pathlib import Path

from intcode import Program


class Robot:
    def __init__(self):
        self.painted = {}
        self.position = 0j
        self.direction = 1j

    def step(self, color, turn):
        assert color in (0, 1)
        self.painted[self.position] = color
        assert turn in (0, 1)
        self.direction *= 1j if turn == 0 else -1j
        self.position += self.direction

    def get_camera(self):
        return self.painted.get(self.position, 0)


def test_sample():
    r = Robot()
    steps = [
        (1, 0),
        (0, 0),
        (1, 0),
        (1, 0),
        (0, 1),
        (1, 0),
        (1, 0),
    ]
    for color, turn in steps:
        r.step(color, turn)
    print(len(r.painted))


def run_robot(data, start_with_white):
    p = Program(data, [], pause_on_output=True)
    r = Robot()
    if start_with_white:
        r.painted[0j] = 1
    while True:
        p.inputs = [r.get_camera()]
        p.outputs.clear()
        p.run_times(2)
        if p.halted:
            break
        color, turn = p.outputs
        r.step(color, turn)
    return r


def solve_easy(data):
    return len(run_robot(data, False).painted)


def solve_hard(data):
    r = run_robot(data, True)
    all_points = {(int(k.real), int(k.imag)): v for k, v in r.painted.items()}
    xs, ys = [p[0] for p in all_points], [p[1] for p in all_points]
    min_x, max_x, min_y, max_y = min(xs), max(xs), min(ys), max(ys)
    lines = []
    for y in range(max_y, min_y - 1, -1):
        lines.append(''.join(('▮' if (all_points.get((x, y)) == 1) else ' ') for x in range(min_x, max_x + 1)))
    return '\n'.join(lines)


def main():
    data = [int(x) for x in Path('11.txt').read_text().split(',') if x]
    print(solve_easy(data))
    print(solve_hard(data))


if __name__ == '__main__':
    main()