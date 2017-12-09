from __future__ import print_function


def has_duplicates(tokens):
    return len(tokens) != len(set(tokens))


def main(file):
    valid_count = 0
    valid_count_hard = 0
    for line in inp:
        tokens = line.rstrip().split()
        if not has_duplicates(tokens):
            valid_count += 1
        canonical_tokens = [''.join(sorted(t)) for t in tokens]
        if not has_duplicates(canonical_tokens):
            valid_count_hard += 1
    print(valid_count, valid_count_hard)


if __name__ == '__main__':
    with open('input.txt') as inp:
        main(inp)
