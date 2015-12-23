import collections
import sys

def execute(program, start_dict):
	idx = 0
	regs = collections.defaultdict(int)
	regs.update(start_dict)

	def apply_to_reg(reg_name, changer):
		regs[reg_name] = changer(regs[reg_name])

	def change_offset(reg_name, offset, reg_tester):
		should_jump = reg_name is None or reg_tester(regs[reg_name])
		real_offset = offset if should_jump else 1
		return idx + real_offset

	while idx < len(program):
		op = program[idx]
		op_name = op[0]
		if op_name.startswith('j'):
			reg_name = op[1] if len(op) >= 3 else None
			offset = op[-1]
			idx = change_offset(reg_name, offset, {
				'jmp': lambda val: True,
				'jie': lambda val: val % 2 == 0,
				'jio': lambda val: val == 1,
			}[op_name])
		else:
			apply_to_reg(op[1], {
				'hlf': lambda val: val/2,
				'tpl': lambda val: val*3,
				'inc': lambda val: val+1,
			}[op_name])
			idx += 1

	print regs
	return regs['b']

def parse_line(line):
	tokens = line.rstrip().replace(',', '').split()
	if tokens[0].startswith('j'):
		tokens[-1] = int(tokens[-1])
	return tokens

program = map(parse_line, sys.stdin)
print execute(program, {})
print execute(program, {'a': 1})