from __future__ import print_function


def main(inp):
    blocks = [int(num.strip()) for num in inp.read().split()]
    seen = {tuple(blocks)}
    iterations = 0
    duplicate = None
    while True:
        max_val, max_idx = None, None
        for idx in xrange(len(blocks)):
            if max_val is None or blocks[idx] > max_val:
                max_val = blocks[idx]
                max_idx = idx
        to_redistribute = blocks[max_idx]
        blocks[max_idx] = 0
        for idx in xrange(to_redistribute):
            blocks[(max_idx + 1 + idx) % len(blocks)] += 1
        iterations += 1
        state = tuple(blocks)
        if duplicate is not None and state == duplicate:
            print('hard', iterations)
            break
        if duplicate is None and state in seen:
            print('easy', iterations)
            duplicate = state
            iterations = 0
        seen.add(state)


if __name__ == '__main__':
    with open('input.txt') as inp:
        main(inp)