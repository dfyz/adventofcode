import sys
import re

target_row, target_col = map(int, re.findall('\d+', sys.stdin.readline()))

num = 20151125
start_row, current_row, current_col = 1, 1, 1
while True:
	current_row = start_row
	current_col = 1
	while current_row >= 1:
		if (current_row, current_col) == (target_row, target_col):
			print num
			sys.exit(0)
		num = num * 252533 % 33554393
		current_row -= 1
		current_col += 1
	start_row += 1
