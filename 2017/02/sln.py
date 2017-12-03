import sys


def main():
    checksum_easy, checksum_hard = 0, 0
    for line in sys.stdin:
        nums = [int(t.strip()) for t in line.rstrip().split()]
        checksum_easy += max(nums) - min(nums)
        checksum_hard += [a // b for a in nums for b in nums if a > b and a % b == 0][0]
    print(checksum_easy, checksum_hard)


if __name__ == "__main__":
    main()
