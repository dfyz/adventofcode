import collections
import operator
import sys

msg = None
replacements = collections.defaultdict(list)
for line in sys.stdin:
	line = line.rstrip()
	tokens = line.split(' => ')
	if len(tokens) == 2:
		replacements[tokens[0]].append(tokens[1])
	else:
		msg = line

def iterate(s):
	results = set()
	for k, v in replacements.iteritems():
		for repl in v:
			start_idx = 0
			while True:
				idx = s.find(k, start_idx)
				if idx < 0:
					break
				res = s[:idx] + repl + s[idx + len(k):]
				results.add(res)
				start_idx = idx + 1
	return results

print len(iterate(msg))