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

	for ss := 1; ss <= S; ss++ {
		ch := rune(springs[ss-1])
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
				if ss < cnt {
					continue
				}
				goodGroup := true
				for delta := 0; goodGroup && delta < cnt; delta++ {
					goodGroup = goodGroup && canBeDamaged(rune(springs[ss-1-delta]))
				}
				if !goodGroup {
					continue
				}
				dpIdx := ss - cnt
				switch {
				case ss == cnt:
					// We are exactly at the start of the string, do nothing.
				case canBeOperational(rune(springs[ss-1-cnt])):
					dpIdx--
				default:
					continue
				}
				// We move to the left by `cnt` damaged springs and possibly one operational spring.
				// And we also decrease the number of groups by one.
				dp[ss][gg] += dp[dpIdx][gg-1]
			}
		}
	}
	return dp[S][G]
}

func main() {
	lines := common.ReadLines("12/input.txt")
	easy := 0
	for _, l := range lines {
		tokens := strings.Split(l, " ")
		groups := make([]int, 0)
		for _, rawGG := range strings.Split(tokens[1], ",") {
			gg, err := strconv.Atoi(rawGG)
			if err != nil {
				log.Fatal(err)
			}
			groups = append(groups, gg)
		}
		easy += countWays(tokens[0], groups)
	}
	fmt.Printf("easy: %d\n", easy)
}