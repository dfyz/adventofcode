import operator


N = 256


def wrap(num):
    return (num + N) % N


def do_round(nums, lengths, idx, skip):
    for l in lengths:
        a, b = idx, wrap(idx + l - 1)
        for _ in xrange(l / 2):
            nums[a], nums[b] = nums[b], nums[a]
            a = wrap(a + 1)
            b = wrap(b - 1)
        idx = wrap(idx + l + skip)
        skip += 1
    return idx, skip


def hash(seq):
    lengths = map(ord, seq) + [17, 31, 73, 47, 23]
    nums = range(N)
    idx, skip = 0, 0
    for _ in xrange(64):
        idx, skip = do_round(nums, lengths, idx, skip)
    return [reduce(operator.xor, nums[start:start + 16]) for start in xrange(0, len(nums), 16)]
