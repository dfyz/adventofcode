package main

import (
	"2023/common"
	"fmt"
	"log"
	"strconv"
	"strings"
)

func canBeOperational(ch rune) bool {
	return ch == '?' || ch == '.'
}

func canBeDamaged(ch rune) bool {
	return ch == '?' || ch == '#'
}

func countWays(springs string, groups []int) int {
	// dp[ss][gg] -- the number of ways if we have S symbols and G groups
	S, G := len(springs), len(groups)
	dp := make([][]int, S+1)
	for ss := 0; ss <= S; ss++ {
		dp[ss] = make([]int, G+1)
	}

	// Exactly one way to get no symbols and no groups
	dp[0][0] = 1
	numDamaged := 0

	for ss := 1; ss <= S; ss++ {
		ch := rune(springs[ss-1])
		if ch == '.' {
			numDamaged = 0
		} else {
			numDamaged++
		}
		// Baseline: we get exactly as many groups as we had at the previous position.
		if canBeOperational(ch) {
			for gg := 0; gg <= G; gg++ {
				dp[ss][gg] = dp[ss-1][gg]
			}
		}
		if canBeDamaged(ch) {
			// This can be a damaged spring. Try to place it in all possible groups.
			for gg := 1; gg <= G; gg++ {
				cnt := groups[gg-1]
				if ss < cnt || numDamaged < cnt {
					continue
				}
				dpIdx := ss - cnt
				if dpIdx < 0 {
					continue
				} else if dpIdx > 0 {
					if !canBeOperational(rune(springs[ss-1-cnt])) {
						continue
					}
					dpIdx--
				}
				// We move to the left by `cnt` damaged springs and possibly one operational spring.
				// And we also decrease the number of groups by one.
				dp[ss][gg] += dp[dpIdx][gg-1]
			}
		}
	}

	return dp[S][G]
}

func multiply(orig, sep string, mulFactor int) string {
	blownUp := make([]string, mulFactor)
	for ii := 0; ii < mulFactor; ii++ {
		blownUp[ii] = orig
	}
	return strings.Join(blownUp, sep)
}

func solveLine(line string, mulFactor int) int {
	tokens := strings.Split(line, " ")

	tokens[0] = multiply(tokens[0], "?", mulFactor)
	tokens[1] = multiply(tokens[1], ",", mulFactor)

	groups := make([]int, 0)
	for _, rawGG := range strings.Split(tokens[1], ",") {
		gg, err := strconv.Atoi(rawGG)
		if err != nil {
			log.Fatal(err)
		}
		groups = append(groups, gg)
	}
	return countWays(tokens[0], groups)
}

func main() {
	lines := common.ReadLines("12/input.txt")
	easy, hard := 0, 0
	for _, l := range lines {
		easy += solveLine(l, 1)
		hard += solveLine(l, 5)
	}
	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
