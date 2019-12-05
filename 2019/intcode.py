import inspect
import itertools


class Program:
    def __init__(self, data, inputs):
        self.data = data
        self.inputs = inputs
        self.outputs = []
        self.ip = 0
        self.dp = 0

    def run(self):
        while (opcode := self.data[self.ip]) != 99:
            op = {
                1: self.add,
                2: self.mul,
                3: self.read_input,
                4: self.print_output,
            }[opcode % 100]

            operand_count = len(inspect.signature(op).parameters)
            operands = self.data[self.ip + 1:self.ip + 1 + operand_count]
            modes = reversed(str(opcode)[:-2])
            parsed_operands = [(o, m == '1') for o, m in itertools.zip_longest(operands, modes)]
            ip_delta = op(*parsed_operands)
            self.ip += ip_delta if ip_delta is not None else 1 + operand_count

    def add(self, in1, in2, out):
        self.set(out, self.get(in1) + self.get(in2))

    def mul(self, in1, in2, out):
        self.set(out, self.get(in1) * self.get(in2))

    def read_input(self, out):
        self.set(out, self.inputs[self.dp])
        self.dp += 1

    def print_output(self, in1):
        self.outputs.append(self.get(in1))

    def get(self, operand):
        pos, mode = operand
        return pos if mode else self.data[pos]

    def set(self, operand, value):
        pos, mode = operand
        assert not mode, 'Immediate mode is not supported in set()'
        self.data[pos] = value
