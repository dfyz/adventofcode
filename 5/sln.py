import sys
import re

def is_nice(s):
	if re.search('ab|cd|pq|xy', s) is not None:
		return False
	if re.search('(.)\\1', s) is None:
		return False
	if len(re.findall('[aeiou]', s)) < 3:
		return False
	return True

answer = 0
for line in sys.stdin:
	if is_nice(line.strip()):
		answer += 1
print answer