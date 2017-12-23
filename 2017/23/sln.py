from __future__ import print_function

import collections
import string


def eval_program(program, a_start):
	registers = collections.defaultdict(int)

	def to_val(s):
		return registers[s] if s in string.letters else int(s)

	registers['a'] = a_start

	idx = 0
	mul_count = 0
	while idx < len(program):
		cmd, op1, op2 = program[idx]
		if cmd == 'set':
			registers[op1] = to_val(op2)
		elif cmd == 'sub':
			registers[op1] -= to_val(op2)
		elif cmd == 'mul':
			mul_count += 1
			registers[op1] *= to_val(op2)
		elif cmd == 'jnz':
			if to_val(op1) != 0:
				idx += to_val(op2)
				continue
		idx += 1
	return registers, mul_count


def is_composite(num):
	x = 2
	while x * x <= num:
		if num % x == 0:
			return True
		x += 1
	return False


def main(inp):
	program = [line.rstrip().split() for line in inp]

	print(eval_program(program, 0)[1])

	program_end = 0
	while program[program_end][1] in ('a', 'b', 'c', '1'):
		program_end += 1
	regs = eval_program(program[:program_end], 1)[0]

	delta = None
	for idx in xrange(len(program) - 1, 0, -1):
		if program[idx][:2] == ['sub', 'b']:
			delta = -int(program[idx][2])
			break

	start_val, end_val = regs['b'], regs['c']
	hard_answer = 0
	for num in xrange(start_val, end_val + 1, delta):
		if is_composite(num):
			hard_answer += 1
	print(hard_answer)


if __name__ == '__main__':
	import sys
	main(sys.stdin)
