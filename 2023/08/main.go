package main

import (
	"2023/common"
	"fmt"
	"strings"
	"unicode"
)

type Fork struct {
	left, right string
}

func main() {
	lines := common.ReadLines("08/input.txt")
	directions := lines[0]

	forks := make(map[string]Fork)
	allSrcs, allDsts := make([]string, 0), make([]string, 0)
	for _, l := range lines[2:] {
		tokens := strings.FieldsFunc(l, func(r rune) bool {
			return !unicode.IsLetter(r) && !unicode.IsDigit(r)
		})
		src, dstLeft, dstRight := tokens[0], tokens[1], tokens[2]
		forks[src] = Fork{dstLeft, dstRight}

		if strings.HasSuffix(src, "A") {
			allSrcs = append(allSrcs, src)
		}
		if strings.HasSuffix(src, "Z") {
			allDsts = append(allDsts, src)
		}
	}

	common.Ensure(len(allSrcs) == len(allDsts), "invalid forks")

	trace := func(exactZ bool, src string) int {
		steps := 0
		cur := src
		for {
			if exactZ {
				if cur == "ZZZ" {
					break
				}
			} else {
				if cur[len(cur)-1] == 'Z' {
					break
				}
			}
			ff, ok := forks[cur]
			common.Ensure(ok, "missing src")
			ch := directions[steps%len(directions)]
			common.Ensure(ch == 'L' || ch == 'R', "invalid directions")
			switch ch {
			case 'L':
				cur = ff.left
			case 'R':
				cur = ff.right
			}
			steps++
		}
		return steps
	}

	easy := trace(true, "AAA")
	hard := uint64(1)
	gcd := func(a, b uint64) uint64 {
		for b != 0 {
			a, b = b, a%b
		}
		return a
	}
	for _, src := range allSrcs {
		steps := uint64(trace(false, src))
		hard = steps * hard / gcd(steps, hard)
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
