package main

import (
	"2023/common"
	"fmt"
)

type P = common.P
type Points = map[P]bool
type Grid = []string

type RowStats struct {
	startsAfter int
	deltas      []int
}

type GridStats = map[int]RowStats

func pythonMod(x, y int) int {
	if x >= 0 {
		return x % y
	}
	return (y - (-x % y)) % y
}

func move(step int, points Points, grid Grid, stats GridStats) Points {
	res := make(Points)
	n := len(grid)

	oldRowCounts, newRowCounts := make(map[int]int), make(map[int]int)
	allRows := make(map[int]bool)

	for p, _ := range points {
		oldRowCounts[p.Y]++
		allRows[p.Y] = true
		for _, delta := range common.Dirs {
			newP := p.Add(delta)
			newRR, newCC := pythonMod(newP.Y, n), pythonMod(newP.X, n)
			adj := grid[newRR][newCC]
			if adj == '.' || adj == 'S' {
				if !res[newP] {
					res[newP] = true
					newRowCounts[newP.Y]++
					allRows[newP.Y] = true
				}
			}
		}
	}

	for row, _ := range allRows {
		delta := newRowCounts[row] - oldRowCounts[row]
		rowStats, ok := stats[row]
		if !ok {
			rowStats.startsAfter = step - 1
		}
		rowStats.deltas = append(rowStats.deltas, delta)
		stats[row] = rowStats
	}

	return res
}

type DeltaLoop struct {
	startsAfter int
	beforeLoop  []int
	cumSums     []int
}

func (d *DeltaLoop) Eval(step int) int {
	count := step - d.startsAfter
	if count <= 0 {
		return 0
	}

	res := 0
	for ii := 0; count > 0 && ii < len(d.beforeLoop); ii++ {
		res += d.beforeLoop[ii]
		count--
	}

	periodLen := len(d.cumSums)
	full := count / periodLen
	res *= full + 1

	res += d.cumSums[periodLen-1] * full * (full + 1) / 2
	count = count % periodLen
	if count > 0 {
		res += (d.cumSums[count-1]) * (full + 1)
	}

	return res
}

func main() {
	grid := common.ReadLines("21/input.txt")
	n := len(grid)
	common.Ensure(n == len(grid[0]), "expected a square grid")
	common.Ensure(grid[n/2][n/2] == 'S', "expected S to be in the middle of the board")

	cur := make(Points)
	cur[P{n / 2, n / 2}] = true
	gridStats := make(GridStats)
	gridStats[n/2] = RowStats{-1, []int{1}}

	for step := 1; step <= 2*n; step++ {
		cur = move(step, cur, grid, gridStats)
	}

	isPeriodic := func(x []int, off int) bool {
		for ii := off; ii < off+n; ii++ {
			for jj := ii + n; jj < len(x); jj += n {
				if x[jj] != x[ii] {
					return false
				}
			}
		}
		return true
	}

	loops := make(map[int]DeltaLoop)

	for rr := n/2 - n + 1; rr < n+n/2; rr++ {
		deltas := gridStats[rr].deltas
		common.Ensure(len(deltas) >= n, "oops")
		beforeLoopCnt := 0
		for !isPeriodic(deltas, beforeLoopCnt) {
			beforeLoopCnt++
		}
		loop := DeltaLoop{
			startsAfter: gridStats[rr].startsAfter,
			beforeLoop:  deltas[:beforeLoopCnt],
			cumSums:     make([]int, n),
		}
		acc := 0
		for ii := 0; ii < n; ii++ {
			acc += deltas[beforeLoopCnt+ii]
			loop.cumSums[ii] = acc
		}
		loops[rr] = loop
	}

	eval := func(steps int) int {
		evalSum := 0
		for rr := n / 2; rr > n/2-n; rr-- {
			l := loops[rr]
			evalSum += l.Eval(steps)
		}
		for rr := n / 2; rr < n+n/2; rr++ {
			l := loops[rr]
			evalSum += l.Eval(steps)
		}
		evalSum -= 1 + steps
		return evalSum
	}

	fmt.Printf("easy: %d\n", eval(64))
	fmt.Printf("hard: %d\n", eval(26501365))
}
