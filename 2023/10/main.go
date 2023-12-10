package main

import (
	"2023/common"
	"fmt"
	"image"
	"strings"
)

type P = image.Point

const N = 4

type AdjMap = map[rune][N]bool

const (
	Left int = iota
	Up
	Right
	Down
)

var deltas = [N]P{
	{-1, 0}, // left
	{0, -1}, // up
	{1, 0},  // right
	{0, 1},  // down
}

func inv(x int) int {
	return (x + 2) % len(deltas)
}

func getAdjMap() AdjMap {
	return AdjMap{
		'-': {Left: true, Right: true},
		'|': {Up: true, Down: true},
		'F': {Down: true, Right: true},
		'L': {Up: true, Right: true},
		'7': {Down: true, Left: true},
		'J': {Up: true, Left: true},
	}
}

func getStartPos(lines []string, rows int, cols int) P {
	for rr := 0; rr < rows; rr++ {
		for cc := 0; cc < cols; cc++ {
			if lines[rr][cc] == 'S' {
				return P{cc, rr}
			}
		}
	}
	panic("No S on the map")
}

func isValidPos(p P, rows, cols int) bool {
	return p.X >= 0 && p.Y >= 0 && p.X < cols && p.Y < rows
}

func main() {
	adjMap := getAdjMap()
	lines := common.ReadLines("10/input.txt")
	rows, cols := len(lines), len(lines[0])
	startPos := getStartPos(lines, rows, cols)
	curPos := startPos
	visited := make(map[P]bool)
	pathLen := 0

	var foundStart string
	for k, v := range adjMap {
		conns := 0
		for ii, dd := range deltas {
			nextPos := curPos.Add(dd)
			if !isValidPos(nextPos, rows, cols) {
				continue
			}
			nextCh := rune(lines[nextPos.Y][nextPos.X])
			if v[ii] && adjMap[nextCh][inv(ii)] {
				conns++
			}
		}
		if conns == 2 {
			foundStart = string(k)
			break
		}
	}

	common.Ensure(foundStart != "", "Failed to find the start symbol")
	lines[curPos.Y] = strings.Replace(lines[curPos.Y], "S", foundStart, -1)

	for {
		visited[curPos] = true
		ch := rune(lines[curPos.Y][curPos.X])
		found := false
		for ii, dd := range deltas {
			if !adjMap[ch][ii] {
				continue
			}
			nextPos := curPos.Add(dd)
			if _, ok := visited[nextPos]; ok {
				continue
			}
			if !isValidPos(nextPos, rows, cols) {
				continue
			}
			nextCh := rune(lines[nextPos.Y][nextPos.X])
			if adjMap[nextCh][inv(ii)] {
				curPos = nextPos
				pathLen++
				found = true
				break
			}
		}
		if !found {
			break
		}
	}

	common.Ensure(pathLen%2 != 0, fmt.Sprintf("even pathLen: %d", pathLen))
	fmt.Printf("easy: %d\n", (pathLen+1)/2)
}
