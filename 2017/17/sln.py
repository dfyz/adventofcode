from __future__ import print_function

import sys


def main(steps):
    nums = [0]
    pos = 0
    for i in xrange(2017):
        pos = (pos + steps) % len(nums)
        nums.insert(pos + 1, i + 1)
        pos += 1
    print(nums[pos + 1])

    after_zero = None
    pos = 0
    count = 1
    for i in xrange(50000000):
        pos = (pos + steps) % count
        if pos == 0:
            after_zero = i + 1
        pos += 1
        count += 1
    print(after_zero)


if __name__ == '__main__':
    main(int(sys.stdin.readline().rstrip()))