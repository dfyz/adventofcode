from __future__ import print_function


def get_steps(offsets, mutator):
    idx = 0
    steps = 0
    offsets = offsets[::]
    while idx < len(offsets):
        delta = offsets[idx]
        mutator(offsets, idx)
        idx += delta
        steps += 1
    return steps


def main(inp):
    offsets = [int(line.rstrip()) for line in inp.readlines()]

    def mutator_easy(offsets, idx):
        offsets[idx] += 1

    def mutator_hard(offsets, idx):
        old_val = offsets[idx]
        offsets[idx] += (1 if old_val < 3 else -1)

    print(get_steps(offsets, mutator_easy))
    print(get_steps(offsets, mutator_hard))


if __name__ == '__main__':
    with open('input.txt') as inp:
        main(inp)
