from __future__ import print_function


def read_garbage(line, idx):
    garbage_count = 0
    while True:
        ch = line[idx]
        if ch == '>':
            return idx, garbage_count
        elif ch == '!':
            idx += 2
        else:
            idx += 1
            garbage_count += 1


def get_score(line, idx, initial_score):
    idx += 1
    result = initial_score
    garbage_count = 0
    while line[idx] != '}':
        ch = line[idx]
        if ch == '{':
            next_idx, next_result, next_garbage_count = get_score(line, idx, initial_score + 1)
            idx = next_idx + 1
            result += next_result
            garbage_count += next_garbage_count
        elif ch == '<':
            next_idx, next_garbage_count = read_garbage(line, idx + 1)
            idx = next_idx + 1
            garbage_count += next_garbage_count
        elif ch == ',':
            idx += 1
    return idx, result, garbage_count


def main(inp):
    for line in inp:
        print(get_score(line.strip(), 0, 1)[1:])


if __name__ == '__main__':
    with open('input.txt') as inp:
        main(inp)
