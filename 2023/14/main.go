package main

import (
	"2023/common"
	"fmt"
	"log"
)

const (
	North = iota
	West
	South
	East
	DirCount
)

func move(board [][]rune, dim int, dir int) {
	get := func(c1, c2 int) *rune {
		var rr, cc int
		switch dir {
		case North:
			rr, cc = c2, c1
		case West:
			rr, cc = c1, c2
		case South:
			rr, cc = dim-1-c2, c1
		case East:
			rr, cc = c1, dim-1-c2
		default:
			log.Fatal("uh oh")
		}
		return &board[rr][cc]
	}
	swap := func(c1, c2, newC2 int) {
		loc1, loc2 := get(c1, c2), get(c1, newC2)
		*loc1, *loc2 = *loc2, *loc1
	}
	for c1 := 0; c1 < dim; c1++ {
		for c2 := 0; c2 < dim; c2++ {
			if *get(c1, c2) != 'O' {
				continue
			}
			newC2 := c2
			for newC2-1 >= 0 && *get(c1, newC2-1) == '.' {
				newC2--
			}
			swap(c1, c2, newC2)
		}
	}
}

func count(board [][]rune) int {
	res := 0
	for rr, row := range board {
		for _, val := range row {
			if val == 'O' {
				res += len(board) - rr
			}
		}
	}
	return res
}

func main() {
	lines := common.ReadLines("14/input.txt")
	dim := len(lines)
	common.Ensure(dim == len(lines[0]), "Expected a square board")

	board := make([][]rune, dim)
	for rr, l := range lines {
		board[rr] = []rune(l)
	}

	var easy, hard int
	scorePos := make([]int, dim*dim*dim)
	for ii := 0; ii < len(scorePos); ii++ {
		scorePos[ii] = -1
	}
	const N = 1000000000
	scores := make([]int, 0)
	for ii := 0; ; ii++ {
		for jj := 0; jj < DirCount; jj++ {
			move(board, dim, jj)
			if ii == 0 && jj == 0 {
				easy = count(board)
			}
		}
		score := count(board)
		if scorePos[score] != -1 && hard == 0 {
			prefixLen, loopLen := scorePos[score], ii-scorePos[score]
			hard = scores[prefixLen+(N-1-prefixLen)%loopLen]
			break
		}
		scorePos[score] = ii
		scores = append(scores, score)
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
