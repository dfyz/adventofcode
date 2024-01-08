package main

import (
	"2023/common"
	"fmt"
)

type P = common.P

func solveEasy(grid []string, nn int, src, dst P) int {
	inGrid := func(p P) bool {
		return p.X >= 0 && p.Y >= 0 && p.X < nn && p.Y < nn
	}

	visited := make(map[P]bool)
	var backtrack func(cur P, dist int) int
	backtrack = func(cur P, dist int) int {
		if cur == dst {
			return dist
		}

		res := 0
		for _, dd := range common.Dirs {
			next := cur.Add(dd)
			if !inGrid(next) {
				continue
			}
			adj := grid[next.Y][next.X]
			if adj == '#' {
				continue
			}
			inc := 1
			if adj == '>' {
				next = next.Add(common.Dirs[common.Right])
				common.Ensure(inGrid(next), "right step fail")
				inc++
			} else if adj == 'v' {
				next = next.Add(common.Dirs[common.Down])
				common.Ensure(inGrid(next), "down step fail")
				inc++
			} else {
				common.Ensure(adj == '.', "oops")
			}

			if !visited[next] {
				visited[next] = true
				res = max(res, backtrack(next, dist+inc))
				visited[next] = false
			}
		}
		return res
	}

	return backtrack(src, 0)
}

func solveHard(grid []string, nn int, src, dst P) int {
	srcIdx, dstIdx := -1, -1

	isFree := func(p P) bool {
		return p.X >= 0 && p.Y >= 0 && p.X < nn && p.Y < nn && grid[p.Y][p.X] != '#'
	}

	isJunction := func(cur P) bool {
		if cur == src || cur == dst {
			return true
		}
		if grid[cur.Y][cur.X] == '#' {
			return false
		}
		cnt := 0
		for _, dd := range common.Dirs {
			next := cur.Add(dd)
			if isFree(next) {
				cnt++
			}
		}
		common.Ensure(cnt >= 2, fmt.Sprintf("(%d, %d) is sus", cur.Y, cur.X))
		return cnt > 2
	}

	junctions := make(map[P]int)
	for rr := 0; rr < nn; rr++ {
		for cc := 0; cc < nn; cc++ {
			cur := P{cc, rr}
			if isJunction(cur) {
				newId := len(junctions)
				if cur == src {
					srcIdx = newId
				} else if cur == dst {
					dstIdx = newId
				}
				junctions[cur] = newId
			}
		}
	}
	common.Ensure(srcIdx != -1 && dstIdx != -1, "achtung")

	juncCnt := len(junctions)
	distMat := make([][]int, juncCnt)
	adj := make([][]int, juncCnt)
	for ii := 0; ii < len(distMat); ii++ {
		distMat[ii] = make([]int, juncCnt)
	}

	for cur, idx := range junctions {
		for dirIdx, dd := range common.Dirs {
			nextDist := 1
			curDir := dirIdx
			next := cur.Add(dd)
			if !isFree(next) {
				continue
			}

			for {
				if _, ok := junctions[next]; ok {
					break
				}

				for nextDir, nextDD := range common.Dirs {
					if (curDir+2)%len(common.Dirs) == nextDir {
						continue
					}

					nextCand := next.Add(nextDD)
					if isFree(nextCand) {
						next = nextCand
						curDir = nextDir
						nextDist++
						break
					}
				}
			}

			nextJuncIdx := junctions[next]
			distMat[idx][nextJuncIdx] = nextDist
			adj[idx] = append(adj[idx], nextJuncIdx)
		}
	}

	visited := make([]bool, juncCnt)
	var backtrack func(juncIdx int, dist int) int
	backtrack = func(curJunc int, dist int) int {
		if curJunc == dstIdx {
			return dist
		}

		res := 0
		for _, junc := range adj[curJunc] {
			hop := distMat[curJunc][junc]
			if hop > 0 && !visited[junc] {
				visited[junc] = true
				res = max(res, backtrack(junc, dist+hop))
				visited[junc] = false
			}
		}
		return res
	}

	visited[srcIdx] = true
	return backtrack(srcIdx, 0)
}

func main() {
	grid := common.ReadLines("23/input.txt")
	nn := len(grid)
	common.Ensure(nn == len(grid[0]), "expected a square grid")

	src, dst := P{1, 0}, P{nn - 2, nn - 1}

	fmt.Printf("easy: %d\n", solveEasy(grid, nn, src, dst))
	fmt.Printf("hard: %d\n", solveHard(grid, nn, src, dst))
}
