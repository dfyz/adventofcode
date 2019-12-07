import inspect
import itertools


ADD = 1
MUL = 2
INPUT = 3
OUTPUT = 4
JIT = 5
JIF = 6
LT = 7
EQ = 8
HALT = 99


class Program:
    def __init__(self, data, inputs, pause_on_output=False):
        self.data = data
        self.inputs = inputs
        self.outputs = []
        self.ip = 0
        self.dp = 0
        self.pause_on_output = pause_on_output
        self.halted = False

    def run(self):
        while (opcode := self.data[self.ip]) != HALT:
            op = {
                ADD: self.add,
                MUL: self.mul,
                INPUT: self.read_input,
                OUTPUT: self.print_output,
                JIT: self.jump_if_true,
                JIF: self.jump_if_false,
                LT: self.less_than,
                EQ: self.equals,
            }[opcode % 100]

            operand_count = len(inspect.signature(op).parameters)
            operands = self.data[self.ip + 1:self.ip + 1 + operand_count]
            modes = reversed(str(opcode)[:-2])
            parsed_operands = [(o, m == '1') for o, m in itertools.zip_longest(operands, modes)]
            if (new_ip := op(*parsed_operands)) is not None:
                self.ip = new_ip
            else:
                self.ip += 1 + operand_count
            if self.pause_on_output and opcode == OUTPUT:
                break
        self.halted = opcode == HALT

    def add(self, in1, in2, out):
        self.set(out, self.get(in1) + self.get(in2))

    def mul(self, in1, in2, out):
        self.set(out, self.get(in1) * self.get(in2))

    def read_input(self, out):
        self.set(out, self.inputs[self.dp])
        self.dp += 1

    def print_output(self, in1):
        self.outputs.append(self.get(in1))

    def jump_if_true(self, in1, in2):
        if self.get(in1) != 0:
            return self.get(in2)

    def jump_if_false(self, in1, in2):
        if self.get(in1) == 0:
            return self.get(in2)

    def less_than(self, in1, in2, out):
        self.set(out, int(self.get(in1) < self.get(in2)))

    def equals(self, in1, in2, out):
        self.set(out, int(self.get(in1) == self.get(in2)))

    def get(self, operand):
        pos, mode = operand
        return pos if mode else self.data[pos]

    def set(self, operand, value):
        pos, mode = operand
        assert not mode, 'Immediate mode is not supported in set()'
        self.data[pos] = value
