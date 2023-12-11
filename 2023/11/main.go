package main

import (
	"2023/common"
	"fmt"
	"image"
)

type P = image.Point

func solve(galaxies []P, expandRows, expandCols []bool, isHard bool) int {
	expansionFactor := 2
	if isHard {
		expansionFactor = 1000000
	}
	getDist := func(src, dst P) int {
		rDist := 0
		for coord := min(src.Y, dst.Y); coord < max(src.Y, dst.Y); coord++ {
			if expandRows[coord] {
				rDist += expansionFactor - 1
			}
			rDist++
		}
		cDist := 0
		for coord := min(src.X, dst.X); coord < max(src.X, dst.X); coord++ {
			if expandCols[coord] {
				cDist += expansionFactor - 1
			}
			cDist++
		}
		return rDist + cDist
	}

	res := 0
	for ii := range galaxies {
		for jj := ii + 1; jj < len(galaxies); jj++ {
			res += getDist(galaxies[ii], galaxies[jj])
		}
	}
	return res
}

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

	galaxies := make([]P, 0)
	for rr := 0; rr < rows; rr++ {
		for cc := 0; cc < cols; cc++ {
			if lines[rr][cc] == '#' {
				galaxies = append(galaxies, P{cc, rr})
			}
		}
	}

	fmt.Printf("easy: %d\n", solve(galaxies, expandRows, expandCols, false))
	fmt.Printf("hard: %d\n", solve(galaxies, expandRows, expandCols, true))
}
