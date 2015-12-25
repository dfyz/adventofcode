import itertools
import operator
import sys

nums = set(map(int, sys.stdin))

def try_split(target, is_hard, leftovers):
	for size in xrange(len(leftovers) / 2 + 1, 0, -1):
		for comb in itertools.combinations(leftovers, size):
			if sum(comb) != target:
				continue
			left_leftovers = leftovers - set(comb)
			if not is_hard:
				return comb, left_leftovers
			candidate = try_split(target, False, left_leftovers)
			if candidate is not None:
				return comb, candidate
	return None

def solve(is_hard):
	target = sum(nums)/(4 if is_hard else 3)
	for count in xrange(1, len(nums)):
		for comb in itertools.combinations(nums, count):
			if sum(comb) == target:
				candidate = try_split(target, is_hard, nums - set(comb))
				if candidate is not None:
					return reduce(operator.mul, comb)

print solve(False)
print solve(True)