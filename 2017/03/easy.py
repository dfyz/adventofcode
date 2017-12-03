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
    for dx, dy in itertools.islice(steps(), step_count - 1):
        x += dx
        y += dy
    print(abs(x) + abs(y))


if __name__ == "__main__":
    main(325489)
