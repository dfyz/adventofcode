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

func main() {
	adjMap := getAdjMap()
	lines := common.ReadLines("10/input.txt")
	rows, cols := len(lines), len(lines[0])
	startPos := getStartPos(lines, rows, cols)
	curPos := startPos
	visited := make(map[P]bool)
	pathLen := 0

	lines[curPos.Y] = strings.Replace(lines[curPos.Y], "S", "F", -1)

	for {
		visited[curPos] = true
		ch := rune(lines[curPos.Y][curPos.X])
		found := false
		for ii, dd := range deltas {
			if !adjMap[ch][ii] {
				continue
			}
			nextPos := P{
				curPos.X + dd.X,
				curPos.Y + dd.Y,
			}
			if _, ok := visited[nextPos]; ok {
				continue
			}
			if nextPos.X < 0 || nextPos.Y < 0 || nextPos.X >= cols || nextPos.Y >= rows {
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
