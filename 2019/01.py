from pathlib import Path


def get_fuel(x):
    return x // 3 - 2


def get_fuel_hard(x):
    result = 0
    while (next_fuel := get_fuel(x)) > 0:
        result += next_fuel
        x = next_fuel
    return result


def solve_easy(data):
    return sum(get_fuel(x) for x in data)


def solve_hard(data):
    return sum(get_fuel_hard(x) for x in data)


def main():
    data = [int(x) for x in Path('01.txt').read_text().split('\n') if x]
    print(solve_easy(data))
    print(solve_hard(data))


if __name__ == '__main__':
    main()
