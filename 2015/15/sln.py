import functools
import operator
import sys
import re

ingredients = []
for line in sys.stdin:
	nums = re.findall('-?\d+', line.rstrip())
	ingredients.append(map(int, nums))

ingredient_count = len(ingredients)
property_count = len(ingredients[0])

def combinations(idx, left, current_list):
	if idx >= ingredient_count:
		if left == 0:
			yield current_list
		return
	for take in xrange(1, left + 1):
		for res in combinations(idx + 1, left - take, current_list + [take]):
			yield res

teaspoons = 100
needed_calory_count = 500

def sum_property(comb, pidx):
	return sum(comb[iidx]*ingredients[iidx][pidx] for iidx in xrange(ingredient_count))

def try_combination(comb, filter_by_calories):
	if filter_by_calories:
		calory_count = sum_property(comb, -1)
		if calory_count != needed_calory_count:
			return 0
	counts = [max(0, sum_property(comb, pidx)) for pidx in xrange(property_count - 1)]
	return reduce(operator.mul, counts)

def get_answer(filter_by_calories):
	return max(try_combination(c, filter_by_calories) for c in combinations(0, teaspoons, []))

print get_answer(False)
print get_answer(True)
