from __future__ import print_function

import collections
import operator


def get_op(str_op):
    return {
        '<': operator.lt,
        '>': operator.gt,
        '==': operator.eq,
        '!=': operator.ne,
        '>=': operator.ge,
        '<=': operator.le,
    }[str_op]


def main(inp):
    instructions = [line.rstrip().split() for line in inp]
    registers = collections.defaultdict(int)
    hard_answer = 0
    for instr in instructions:
        op = get_op(instr[5])
        if op(registers[instr[4]], int(instr[6])):
            sgn = {'inc': 1, 'dec': -1}[instr[1]]
            registers[instr[0]] += sgn * int(instr[2])
            candidate = registers[instr[0]]
            if candidate > hard_answer:
                hard_answer = candidate
    print(max(registers.itervalues()))
    print(hard_answer)


if __name__ == '__main__':
    with open('input.txt') as inp:
        main(inp)
