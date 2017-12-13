from __future__ import print_function


def get_severity(data, delay):
    caught, result = False, 0
    for d, r in data:
        if (delay + d) % (2 * (r - 1)) == 0:
            caught = True
            result += d * r
    return caught, result


def main(inp):
    data = [map(int, line.rstrip().split(': ')) for line in inp]
    print(get_severity(data, 0)[1])
    hard_answer = 0
    while get_severity(data, hard_answer)[0]:
        hard_answer += 1
    print(hard_answer)


if __name__ == '__main__':
    import sys
    main(sys.stdin)