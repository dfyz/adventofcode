import collections
import sys
from pathlib import Path


def parse_input(input):
    result = collections.defaultdict(list)
    for line in input.strip().split('\n'):
        parent, child = line.split(')')
        result[parent].append(child)
    return result


COM = 'COM'


def solve_easy(data):
    def traverse(root, height):
        return height + sum(traverse(v, height + 1) for v in data[root])
    return traverse(COM, 0)


def solve_hard(data):
    def traverse(root):
        if root == 'YOU':
            return None, 0, None
        elif root == 'SAN':
            return None, None, 0
        child_answers = [traverse(v) for v in data[root]]
        def unpack_child_answers(idx):
            return min((val for ans in child_answers if (val := ans[idx]) is not None), default=None)
        answer, you_dist, san_dist = [unpack_child_answers(idx) for idx in range(3)]
        if you_dist is not None:
            you_dist += 1
        if san_dist is not None:
            san_dist += 1
        if you_dist is not None and san_dist is not None:
            new_answer = you_dist + san_dist
            if answer is None or new_answer < answer:
                answer = new_answer
        return answer, you_dist, san_dist
    return traverse(COM)[0] - 2


def test_sample():
    easy_data = parse_input('''
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
    ''')
    print(solve_easy(easy_data))
    hard_data = parse_input('''
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN
    ''')
    print(solve_hard(hard_data))


def main():
    data = parse_input(Path('06.txt').read_text())
    sys.setrecursionlimit(len(data))
    print(solve_easy(data))
    print(solve_hard(data))


if __name__ == '__main__':
    main()