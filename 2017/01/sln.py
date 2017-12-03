import sys


def get_answer(line, delta):
    return sum(int(line[idx]) for idx in range(len(line)) if line[idx] == line[(idx + delta) % len(line)])

def main():
    line = sys.stdin.readline().rstrip()
    print(get_answer(line, 1))
    print(get_answer(line, len(line) // 2))


if __name__ == "__main__":
    main()
