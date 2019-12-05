import itertools
import operator


def run_program(data, inputs):
    ip = 0

    while True:
        opcode = data[ip]
        if opcode == 99:
            return
        else:
            def read_input():
                return inputs[0]

            op, operand_count, has_output = {
                1: (operator.add, 3, True),
                2: (operator.mul, 3, True),
                3: (read_input, 1, True),
                4: (print, 1, False),
            }[opcode % 100]
            opcode //= 100

            out_pos = None
            operands = data[ip + 1:ip + 1 + operand_count]
            if has_output:
                out_pos = operands[-1]
                operands = operands[:-1]

            modes = reversed(str(opcode)) if opcode else []
            parsed_operands = [o if m == '1' else data[o] for o, m in itertools.zip_longest(operands, modes)]
            result = op(*parsed_operands)
            if has_output:
                data[out_pos] = result
        ip += 1 + operand_count