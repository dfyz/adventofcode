from pathlib import Path


def parse_trace(trace):
    direction_map = {
        'U': 1j,
        'D': -1j,
        'L': -1,
        'R': 1,
    }
    return [(direction_map[x[0]], int(x[1:])) for x in trace.split(',')]


def get_fingerprint(trace):
    pos = 0j
    step_count = 0
    fingerprint = {}
    for direction, count in trace:
        for _ in range(count):
            pos += direction
            step_count += 1
            if pos not in fingerprint:
                fingerprint[pos] = step_count
    return fingerprint


def manhattan(z):
    return int(abs(z.real) + abs(z.imag))


def solve(trace1, trace2, is_easy):
    fp1, fp2 = get_fingerprint(trace1), get_fingerprint(trace2)
    key = manhattan if is_easy else lambda x: fp1[x] + fp2[x]
    return min(map(key, fp1.keys() & fp2.keys()))


def test_sample():
    samples = [
        ('R8,U5,L5,D3', 'U7,R6,D4,L4'),
        ('R75,D30,R83,U83,L12,D49,R71,U7,L72', 'U62,R66,U55,R34,D71,R55,D58,R83'),
        ('R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51', 'U98,R91,D20,R16,D67,R40,U7,R15,U6,R7'),
    ]
    for line1, line2 in samples:
        line1, line2 = parse_trace(line1), parse_trace(line2)
        print(solve(line1, line2, True))
        print(solve(line1, line2, False))


def main():
    line1, line2 = [parse_trace(x) for x in Path('03.txt').read_text().split('\n') if x]
    print(solve(line1, line2, True))
    print(solve(line1, line2, False))


if __name__ == '__main__':
    main()