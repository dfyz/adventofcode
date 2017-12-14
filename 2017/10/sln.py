from __future__ import print_function


def main(inp):
    import sys
    sys.path.append('..')
    import knot_hash

    seq = inp.read().rstrip()

    easy_lengths = map(int, seq.split(','))
    nums = range(knot_hash.N)
    knot_hash.do_round(nums, easy_lengths, 0, 0)
    print(nums[0] * nums[1])

    print(''.join('{:02X}'.format(elem) for elem in knot_hash.hash(seq)).lower())


if __name__ == '__main__':
    with open('input.txt') as inp:
        main(inp)
