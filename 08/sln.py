import string
import sys

def content_length(s):
	s = s[1:-1]
	idx = 0
	result = 0
	while idx < len(s):
		ch = s[idx]
		result += 1
		idx += 1
		if ch == '\\':
			if s[idx] in '"\\':
				idx += 1
			else:
				assert s[idx] == 'x'
				assert s[idx + 1] in string.hexdigits
				assert s[idx + 2] in string.hexdigits
				idx += 3
	return result

def encoded_content_length(s):
	return 2 + sum(2 if ch in '"\\' else 1 for ch in s)

easy_answer = 0
hard_answer = 0
for line in sys.stdin:
	line = line.strip()
	line_len = len(line)
	easy_answer += line_len - content_length(line)
	hard_answer += encoded_content_length(line) - line_len
print easy_answer, hard_answer