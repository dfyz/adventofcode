import math
import sys

n = 36000000
is_hard = sys.argv[1] == 'hard'

def is_good_other_div(div):
	return div <= 50 if is_hard else True

def present_count(num):
	div = 1
	res = 0
	while div**2 <= num:
		if num % div == 0:
			other_div = num / div
			if is_good_other_div(other_div):
				res += div
			if other_div != div and is_good_other_div(div):
				res += other_div
		div += 1
	return res * (11 if is_hard else 10)

house = 1
while True:
	pc = present_count(house)
	if pc >= n:
		print house
		break
	house += 1
