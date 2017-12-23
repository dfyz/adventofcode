from __future__ import print_function

import collections
import string


def main(inp):
	program = [line.rstrip().split() for line in inp]
	registers = collections.defaultdict(int)

	def to_val(s):
		return registers[s] if s in string.letters else int(s)

	idx = 0
	easy_answer = 0
	while idx < len(program):
		cmd, op1, op2 = program[idx]
		if cmd == 'set':
			registers[op1] = to_val(op2)
		elif cmd == 'sub':
			registers[op1] -= to_val(op2)
		elif cmd == 'mul':
			easy_answer += 1
			registers[op1] *= to_val(op2)
		elif cmd == 'jnz':
			if to_val(op1) != 0:
				idx += to_val(op2)
				continue
		idx += 1
	print(easy_answer)


if __name__ == '__main__':
	import sys
	main(sys.stdin)
