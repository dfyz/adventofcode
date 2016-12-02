import collections
import itertools
import re
import time
import sys

rules = collections.defaultdict(list)

msg = None
for line in sys.stdin:
	line = line.rstrip()
	tokens = line.split(' => ')
	lhs = tokens[0]
	msg = lhs
	if len(tokens) == 2:
		rhs = filter(len, re.split('([A-Z][a-z]*)', tokens[1]))
		rules[lhs].append(rhs)

non_terminals = set(rules)
cache = {}

def is_terminal(symb):
	return symb not in non_terminals

def best_answer(ans1, ans2):
	if ans1 == None:
		return ans2
	if ans2 == None:
		return ans1
	return min(ans1, ans2)

def solve_non_cached(lhs, start, end):
	if ''.join(lhs) == msg[start:end]:
		return 0

	result = None

	if len(lhs) == 1:
		for rhs in rules[lhs[0]]:
			new_result = solve(rhs, start, end)
			if new_result is not None:
				result = best_answer(result, new_result + 1)
	else:
		for lhs_split in xrange(1, len(lhs)):
			for msg_split in xrange(start + 1, end):
				sub_ans_1 = solve(lhs[:lhs_split], start, msg_split)
				if sub_ans_1 is not None:
					sub_ans_2 = solve(lhs[lhs_split:], msg_split, end)
					if sub_ans_2 is not None:
						result = best_answer(result, sub_ans_1 + sub_ans_2)

	return result

def solve(lhs, start, end):
	global cache
	state = (tuple(lhs), start, end)
	if state not in cache:
		cache[state] = solve_non_cached(lhs, start, end)
	return cache[state]

print solve(['e'], 0, len(msg))