def connect(pin, num):
    for idx in range(2):
        if pin[idx] == num:
            return pin[1 - idx]
    return None


def main(inp):
    pins = [[int(x) for x in line.rstrip().split('/')] for line in inp]
    used = [False] * len(pins)

    easy_answer = 0
    hard_answer = [0, 0]

    def backtrack(needed, strength, length):
        prev_length = hard_answer[0]
        if length >= prev_length:
            prev_strength = 0 if length > prev_length else hard_answer[1]
            new_strength = max(prev_length, strength)
            hard_answer.clear()
            hard_answer.extend([length, new_strength])

        result = strength
        for start_idx in range(len(pins)):
            if used[start_idx]:
                continue
            other = connect(pins[start_idx], needed)
            if other is not None:
                used[start_idx] = True
                new_strength = strength + sum(pins[start_idx])
                new_length = length + 1
                new_answer = backtrack(other, new_strength, new_length)
                result = max(result, new_answer)
                used[start_idx] = False
        return result

    easy_answer = backtrack(0, 0, 0)
    print(easy_answer, hard_answer[1])

if __name__ == '__main__':
    import sys
    main(sys.stdin)
