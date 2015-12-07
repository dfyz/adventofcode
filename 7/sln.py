import operator
import string
import sys

solve_hard = False

def fix_name(name):
    return name.upper()

def call_if_needed(s):
    if all(ch in string.digits for ch in s):
        return s
    return '{}()'.format(fix_name(s))

def to_code(tokens, func_name):
    if len(tokens) == 1:
        if solve_hard and func_name == 'B':
            return '956'
        return call_if_needed(tokens[0])

    if len(tokens) == 2:
        assert(tokens[0]) == 'NOT'
        return '~({})'.format(call_if_needed(tokens[1]))

    assert len(tokens) == 3
    op = {
        'AND': 'operator.and_',
        'OR': 'operator.or_',
        'LSHIFT': 'operator.lshift',
        'RSHIFT': 'operator.rshift',
    }[tokens[1]]
    return '{}({}, {})'.format(op, call_if_needed(tokens[0]), call_if_needed(tokens[2]))

program_lines = ['cache = {}']
for line in sys.stdin:
    tokens = line.strip().split()
    func_name = fix_name(tokens[-1])
    program_lines.append('def {}():'.format(func_name))
    program_lines.append('  global cache')
    program_lines.append('  if "{0}" in cache: return cache["{0}"]'.format(func_name))
    program_lines.append('  ans = {}'.format(to_code(tokens[:-2], func_name)))
    program_lines.append('  cache["{}"] = ans'.format(func_name))
    program_lines.append('  return ans')

program_lines.append('print A()')
program = '\n'.join(program_lines)
exec(program)
