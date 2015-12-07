import string
import sys

BIT_COUNT = 16
solve_hard = True

def to_bit_string(num):
    return bin(num)[2:].rjust(BIT_COUNT, '0')

def from_bit_string(bits):
    return int(bits, 2)

def bit_zip(x, y, zipper):
    result_bits = ''.join([zipper(a, b) for a, b in zip(to_bit_string(x), to_bit_string(y))])
    return from_bit_string(result_bits)

def bit_and(x, y):
    return bit_zip(x, y, lambda x, y: '1' if x == '1' and y == '1' else '0')

def bit_or(x, y):
    return bit_zip(x, y, lambda x, y: '1' if x == '1' or y == '1' else '0')

def bit_not(x):
    return bit_zip(x, 0, lambda x, _: '1' if x == '0' else '0')

def bit_lshift(x, offset):
    bits = to_bit_string(x)
    return from_bit_string(bits[offset:].ljust(BIT_COUNT, '0'))

def bit_rshift(x, offset):
    bits = to_bit_string(x)
    return from_bit_string(bits[:BIT_COUNT - offset].rjust(BIT_COUNT, '0'))

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
        return 'bit_not({})'.format(call_if_needed(tokens[1]))

    assert len(tokens) == 3
    op = {
        'AND': 'bit_and',
        'OR': 'bit_or',
        'LSHIFT': 'bit_lshift',
        'RSHIFT': 'bit_rshift',
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
