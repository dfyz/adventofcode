import numpy as np
import sys

n = int(sys.argv[1])
nums = sorted(int(line.rstrip()) for line in sys.stdin)

dp = np.zeros((n + 1, len(nums) + 1, len(nums) + 1))
dp[0, 0, 0] = 1

for num in xrange(1, n + 1):
	for num_count in xrange(1, len(nums) + 1):
		for last_idx in xrange(1, len(nums) + 1):
			prev_num = num - nums[last_idx - 1]
			if prev_num >= 0:
				for prev_last_idx in xrange(0, last_idx):
					dp[num, num_count, last_idx] += dp[prev_num, num_count - 1, prev_last_idx]

last_slice = dp[n]
print np.sum(last_slice)
print np.sum(last_slice[np.sum(last_slice, axis=1).nonzero()][0])
