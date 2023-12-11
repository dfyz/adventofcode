package main

import (
	"2023/common"
	"fmt"
	"image"
)

type P = image.Point

func main() {
	lines := common.ReadLines("11/input.txt")
	rows, cols := len(lines), len(lines[0])
	expandRows, expandCols := make([]bool, rows), make([]bool, cols)

	for rr := 0; rr < rows; rr++ {
		expandRows[rr] = true
		for cc := 0; expandRows[rr] && cc < cols; cc++ {
			expandRows[rr] = expandRows[rr] && lines[rr][cc] == '.'
		}
	}

	for cc := 0; cc < cols; cc++ {
		expandCols[cc] = true
		for rr := 0; expandCols[cc] && rr < rows; rr++ {
			expandCols[cc] = expandCols[cc] && lines[rr][cc] == '.'
		}
	}

	galaxyMap := make(map[P]bool)
	for rr := 0; rr < rows; rr++ {
		for cc := 0; cc < cols; cc++ {
			if lines[rr][cc] == '#' {
				galaxyMap[P{cc, rr}] = true
			}
		}
	}

	galaxies := make([]P, 0)
	for k, _ := range galaxyMap {
		galaxies = append(galaxies, k)
	}

	getDist := func(src, dst P) int {
		rDist := 0
		for coord := min(src.Y, dst.Y); coord < max(src.Y, dst.Y); coord++ {
			if expandRows[coord] {
				rDist += 1000000 - 1
			}
			rDist++
		}
		cDist := 0
		for coord := min(src.X, dst.X); coord < max(src.X, dst.X); coord++ {
			if expandCols[coord] {
				cDist += 1000000 - 1
			}
			cDist++
		}
		return rDist + cDist
	}

	easy := 0
	for ii := range galaxies {
		for jj := ii + 1; jj < len(galaxies); jj++ {
			easy += getDist(galaxies[ii], galaxies[jj])
		}
	}
	fmt.Printf("easy: %d\n", easy)
}
