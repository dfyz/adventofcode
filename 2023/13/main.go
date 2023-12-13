package main

import (
	"2023/common"
	"fmt"
	"math/bits"
)

func solve(board []string, targetDiff int) int {
	rows, cols := len(board), len(board[0])
	rowMasks, colMasks := make([]uint, rows), make([]uint, cols)
	for rr := 0; rr < rows; rr++ {
		for cc := 0; cc < cols; cc++ {
			if board[rr][cc] == '#' {
				rowMasks[rr] |= 1 << cc
				colMasks[cc] |= 1 << rr
			}
		}
	}

	findSymmetry := func(masks []uint) int {
		nn := len(masks)
		for prefix := 1; prefix < nn; prefix++ {
			ii, jj := prefix-1, prefix
			diff := 0
			for diff <= targetDiff && ii >= 0 && jj < nn {
				diff += bits.OnesCount(masks[ii] ^ masks[jj])
				ii--
				jj++
			}
			if diff == targetDiff {
				return prefix
			}
		}
		return 0
	}

	if res := findSymmetry(rowMasks); res > 0 {
		return 100 * res
	}
	res := findSymmetry(colMasks)
	common.Ensure(res > 0, fmt.Sprintf("Failed to find symmetry for %v", board))
	return res
}

func main() {
	lines := common.ReadLines("13/input.txt")
	easy, hard := 0, 0
	board := make([]string, 0)
	for ii, l := range lines {
		if len(l) > 0 {
			board = append(board, l)
			if ii+1 >= len(lines) || len(lines[ii+1]) == 0 {
				easy += solve(board, 0)
				hard += solve(board, 1)
				board = nil
			}
		}
	}
	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
