import sys
sys.setrecursionlimit(10**8)

r7 = None
cache = {}

# The recursive version (segfaults on large inputs).
def do(r0, r1):
	global r7, cache
	assert r0 >= 0 and r1 >= 0 and r7 >= 0

	state = (r0, r1)
	if state in cache:
		return cache[state]

	if r0 == 0:
		return (r1 + 1) % 32768
	ans = do(r0 - 1, r7 if r1 == 0 else do(r0, r1 - 1))
	cache[state] = ans
	return ans

N = 32768
# The non-recursive version (works on every input).
def compute(target_r0, target_r1, target_r7):
	dp = [[0 for r1 in xrange(N)] for r0 in xrange(target_r0 + 1)]

	for r1 in xrange(N):
		dp[0][r1] = (r1 + 1) % 32768

	for r0 in xrange(1, target_r0 + 1):
		dp[r0][0] = dp[r0 - 1][target_r7]
		for r1 in xrange(1, N):
			dp[r0][r1] = dp[r0 - 1][dp[r0][r1 - 1]]
	return dp[target_r0][target_r1]

# 25000 was a lucky guess; the answer is pretty close to it.
for i in xrange(25000, N):
	res = compute(4, 1, i)
	print i, res
	if res == 6:
		break