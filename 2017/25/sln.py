import collections


def read_last_token(inp):
    line = inp.readline().rstrip()
    return line.split()[-1][:-1] if line else None


def main(inp):
    current_state = read_last_token(inp)
    steps = int(inp.readline().rstrip().split()[-2])

    machine = {}

    while True:
        inp.readline()
        from_state = read_last_token(inp)
        if from_state is None:
            break
        machine[from_state] = {}
        for _ in range(2):
            prev_val, next_val, direction, next_state = [
                read_last_token(inp) for _ in range(4)
            ]
            machine[from_state][int(prev_val)] = (
                int(next_val),
                1 if direction == 'right' else -1,
                next_state
            )

    idx = 0
    tape = collections.defaultdict(int)
    for it in range(steps):
        next_val, direction, next_state = machine[current_state][tape[idx]]
        tape[idx] = next_val
        idx += direction
        current_state = next_state
    print(sum(tape.values()))


if __name__ == '__main__':
    import sys
    main(sys.stdin)
