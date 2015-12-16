import collections
import operator
import sys

message = '''children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1'''

delim = ': '
max_count = 10
sue_props = collections.defaultdict(dict)

for line in sys.stdin:
	line = line.rstrip()
	colon_pos = line.index(delim)
	sue_id = line[:colon_pos]
	for sue_key in line[colon_pos + 2:].split(', '):
		item, count = sue_key.split(delim)
		sue_props[sue_id][item] = count

needed_props = [prop.split(delim) for prop in message.split('\n') if prop]

def is_good(key, expected_value, props, is_hard):
	if key not in props:
		return True
	actual_value = props[key]
	if is_hard:
		if key in ['cats', 'trees']:
			return actual_value > expected_value
		if key in ['pomeranians', 'goldfish']:
			return actual_value < expected_value
	return actual_value == expected_value

def solve(is_hard):
	result = []
	for sue_id, props in sue_props.iteritems():
		if all(is_good(k, v, props, is_hard) for k, v in needed_props):
			result.append(sue_id)
	return result

print map(solve, [False, True])