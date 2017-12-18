from __future__ import print_function

import collections
import string


class Program:
    def __init__(self, program, program_id, queues):
        self.registers = collections.defaultdict(int)
        self.registers['p'] = program_id
        self.queues = queues
        self.program = program
        self.idx = 0
        self.program_id = program_id
        self.send_count = 0
        self.first_sent = None
        self.first_printed = False

    def get_id(self):
        return self.program_id

    def step(self):
        def val(s):
            if s in string.letters:
                return self.registers[s]
            return int(s)

        while self.idx < len(self.program):
            i = self.program[self.idx]
            cmd = i[0]
            if cmd == 'set':
                self.registers[i[1]] = val(i[2])
            elif cmd == 'add':
                self.registers[i[1]] += val(i[2])
            elif cmd == 'mul':
                self.registers[i[1]] *= val(i[2])
            elif cmd == 'mod':
                self.registers[i[1]] %= val(i[2])
            elif cmd == 'jgz':
                if val(i[1]) > 0:
                    self.idx += val(i[2])
                    continue
            elif cmd == 'snd':
                to_send = val(i[1])
                self.queues[1 - self.program_id].append(to_send)
                self.send_count += 1
                if self.program_id == 0:
                    self.first_sent = to_send
            elif cmd == 'rcv':
                reg_ref = i[1]
                if self.program_id == 0 and self.registers[reg_ref] and not self.first_printed:
                    print('easy', self.first_sent)
                    self.first_printed = True
                if not self.queues[self.program_id]:
                    return
                self.registers[reg_ref] = self.queues[self.program_id].pop(0)
            self.idx += 1


def main(inp):
    instrs = [line.rstrip().split() for line in inp]
    queues = [[], []]
    progs = [Program(instrs, 0, queues), Program(instrs, 1, queues)]
    idx = 0
    while True:
        progs[idx].step()
        idx = (idx + 1) % len(progs)
        if not queues[0] and not queues[1]:
            print('hard', progs[1].send_count)
            break


if __name__ == '__main__':
    import sys
    main(sys.stdin)