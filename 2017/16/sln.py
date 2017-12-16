from __future__ import print_function

import string


N = 16


def dance(letters, instrs):
    for instr in instrs:
        prefix, rest = instr[0], instr[1:]
        if prefix == 's':
            cnt = int(rest)
            letters = letters[-cnt:] + letters[:N - cnt]
        elif prefix == 'x':
            a, b = map(int, rest.split('/'))
            letters[a], letters[b] = letters[b], letters[a]
        elif prefix == 'p':
            a, b = rest.split('/')
            a_pos, b_pos = letters.index(a), letters.index(b)
            letters[a_pos], letters[b_pos] = letters[b_pos], letters[a_pos]
    return letters


def to_str(letters):
    return ''.join(letters)


def main(inp):
    letters = list(string.ascii_letters[:N])
    instrs = inp.readline().rstrip().split(',')

    states = [to_str(letters)]
    cycle_length = 0
    while True:
        letters = dance(letters, instrs)
        new_state = to_str(letters)
        if new_state == states[0]:
            break
        if len(states) == 1:
            print('easy', new_state)
        states.append(new_state)
    print('hard', states[1000000000 % len(states)])


if __name__ == '__main__':
    import sys
    main(sys.stdin)