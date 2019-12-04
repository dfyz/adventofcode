from pathlib import Path
import itertools


def is_good_password(n, is_easy):
    chars = list(str(n))
    is_sorted = chars == sorted(chars)
    is_good_group = lambda group_size: group_size >= 2 if is_easy else group_size == 2
    has_repeats = any(is_good_group(len(list(group))) for _, group in itertools.groupby(chars))
    return is_sorted and has_repeats


def count_passwords(lower_bound, upper_bound, is_easy):
    return sum(is_good_password(x, is_easy) for x in range(lower_bound, upper_bound + 1))


def test_sample():
    for x in (111111, 223450, 123789, 112233, 123444, 111122):
        print(x, is_good_password(x, True))
        print(x, is_good_password(x, False))


def main():
    lower_bound, upper_bound = map(int, Path('04.txt').read_text().split('-'))
    print(count_passwords(lower_bound, upper_bound, True))
    print(count_passwords(lower_bound, upper_bound, False))


if __name__ == '__main__':
    main()