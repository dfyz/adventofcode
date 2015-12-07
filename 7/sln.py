import operator
import string
import sys

def fix_name(name):
    return name.upper()

def call_if_needed(s):
    if all(ch in string.digits for ch in s):
        return s
    return '{}()'.format(fix_name(s))

def to_code(tokens, func_name):
    if len(tokens) == 1:
        return call_if_needed(tokens[0])

    if 'NOT' in tokens:
        return '~({})'.format(call_if_needed(tokens[1]))

    a, op, b = tokens
    op = {
        'AND': 'operator.and_',
        'OR': 'operator.or_',
        'LSHIFT': 'operator.lshift',
        'RSHIFT': 'operator.rshift',
    }[op]
    return '{}({}, {})'.format(op, call_if_needed(a), call_if_needed(b))

program_template = '''
cache = {{}}
{body}
print A()
'''

wire_template = '''
def {func_name}():
    if "{func_name}" in cache:
        return cache["{func_name}"]
    ans = {code}
    cache["{func_name}"] = ans
    return ans
'''

program_body = ''
for line in sys.stdin:
    tokens = line.strip().split()
    func_name = fix_name(tokens[-1])
    code = to_code(tokens[:-2], func_name)
    program_body += wire_template.format(func_name=func_name, code=code)

exec(program_template.format(body=program_body))
