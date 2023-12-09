package main

import (
	"2023/common"
	"fmt"
	"slices"
)

func solve(seq []int) (int, int) {
	accs := [][2]int{
		{seq[0], seq[len(seq)-1]},
	}

	nonZero := true
	seqLen := len(seq)
	for nonZero {
		nonZero = false
		newAccs := [2]int{}
		for ii := 0; ii+1 < seqLen; ii++ {
			diff := seq[ii+1] - seq[ii]
			seq[ii] = diff
			if diff != 0 {
				nonZero = true
			}
			if ii == 0 {
				newAccs[0] = diff
			} else if ii+2 == seqLen {
				newAccs[1] = diff
			}
		}
		seqLen--
		if nonZero {
			accs = append(accs, newAccs)
		}
	}

	slices.Reverse(accs)
	incrLeft, incrRight := 0, 0
	for _, aa := range accs {
		incrLeft = aa[0] - incrLeft
		incrRight += aa[1]
	}
	return incrRight, incrLeft
}

func main() {
	easy, hard := 0, 0
	for _, l := range common.ReadLines("09/input.txt") {
		nums := common.ParseInts(l)
		easyAns, hardAns := solve(nums)
		easy += easyAns
		hard += hardAns
	}
	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
