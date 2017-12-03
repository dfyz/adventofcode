import itertools


def steps():
    dirs = [(1, 0), (0, -1), (-1, 0), (0, 1)]
    cnt = 1
    inc = False
    for d in itertools.cycle(dirs):
        yield from itertools.repeat(d, cnt)
        if inc:
            cnt += 1
        inc = not inc


def main(step_count):
    x, y = 0, 0
    coords_to_vals = {(0, 0): 1}
    offsets = [-1, 0, 1]
    hard_answer = None
    for idx, (dx, dy) in enumerate(itertools.islice(steps(), step_count - 1), 1):
        if hard_answer is None:
            cur_sum = sum(
                coords_to_vals.get((x + ddx, y + ddy), 0)
                for ddx in offsets
                for ddy in offsets
            )
            coords_to_vals[(x, y)] = cur_sum
            if cur_sum > step_count:
                hard_answer = cur_sum
        x += dx
        y += dy
    print(f'easy: {abs(x) + abs(y)}')
    print(f'hard: {hard_answer}')


if __name__ == "__main__":
    main(325489)
