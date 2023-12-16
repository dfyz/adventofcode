package main

import (
	"2023/common"
	"fmt"
	"image"
)

const (
	Right = iota
	Down
	Left
	Up
)

var Dirs = []P{
	Right: {1, 0},
	Down:  {0, 1},
	Left:  {-1, 0},
	Up:    {0, -1},
}

type P = image.Point

type Pos struct {
	coords P
	dir    int
}

func (self *Pos) move(newDir int) Pos {
	return Pos{self.coords.Add(Dirs[newDir]), newDir}
}

func rot(dd int, delta int) int { return (dd + delta + len(Dirs)) % len(Dirs) }
func rotl(dd int) int           { return rot(dd, -1) }
func rotr(dd int) int           { return rot(dd, 1) }
func isHoriz(dd int) bool       { return dd%2 == 0 }

func countEnergized(board []string, dim int, initial Pos) int {
	visited, energized := make(map[Pos]bool), make(map[P]bool)

	var visit func(start Pos)
	visit = func(start Pos) {
		cc, dd := start.coords, start.dir
		if cc.X < 0 || cc.Y < 0 || cc.X >= dim || cc.Y >= dim {
			// Off the grid, bailing out.
			return
		}
		if _, ok := visited[start]; ok {
			// Have already been there, bailing out.
			return
		}
		visited[start] = true
		energized[cc] = true

		ch := board[cc.Y][cc.X]
		switch ch {
		case '|', '-':
			if (ch == '|') == isHoriz(dd) {
				// The beam should be split into two beams.
				visit(start.move(rotl(dd)))
				visit(start.move(rotr(dd)))
				return
			}
		case '/', '\\':
			if (ch == '/') == isHoriz(dd) {
				dd = rotl(dd)
			} else {
				dd = rotr(dd)
			}
		default:
			common.Ensure(ch == '.', fmt.Sprintf("unexpected symbol: %c", ch))
		}

		visit(start.move(dd))
	}

	visit(initial)
	return len(energized)
}

func main() {
	board := common.ReadLines("16/input.txt")
	dim := len(board)
	common.Ensure(dim == len(board[0]), "expected a square board")

	easy := countEnergized(board, dim, Pos{P{0, 0}, Right})

	hard := 0
	for ii := 0; ii < dim; ii++ {
		hard = max(
			hard,
			countEnergized(board, dim, Pos{P{0, ii}, Right}),
			countEnergized(board, dim, Pos{P{ii, 0}, Down}),
			countEnergized(board, dim, Pos{P{dim - 1, ii}, Left}),
			countEnergized(board, dim, Pos{P{ii, dim - 1}, Up}),
		)
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
