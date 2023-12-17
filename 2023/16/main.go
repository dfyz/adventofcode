package main

import (
	cm "2023/common"
	"fmt"
)

func isHoriz(dd int) bool { return dd%2 == 0 }

func countEnergized(board []string, dim int, initial cm.Pos) int {
	visited, energized := make(map[cm.Pos]bool), make(map[cm.P]bool)

	var visit func(start cm.Pos)
	visit = func(start cm.Pos) {
		cc, dd := start.Coords, start.Dir
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
				visit(start.Move(cm.RotL(dd)))
				visit(start.Move(cm.RotR(dd)))
				return
			}
		case '/', '\\':
			if (ch == '/') == isHoriz(dd) {
				dd = cm.RotL(dd)
			} else {
				dd = cm.RotR(dd)
			}
		default:
			cm.Ensure(ch == '.', fmt.Sprintf("unexpected symbol: %c", ch))
		}

		visit(start.Move(dd))
	}

	visit(initial)
	return len(energized)
}

func main() {
	board := cm.ReadLines("16/input.txt")
	dim := len(board)
	cm.Ensure(dim == len(board[0]), "expected a square board")

	easy := countEnergized(board, dim, cm.Pos{cm.P{0, 0}, cm.Right})

	hard := 0
	for ii := 0; ii < dim; ii++ {
		hard = max(
			hard,
			countEnergized(board, dim, cm.Pos{cm.P{0, ii}, cm.Right}),
			countEnergized(board, dim, cm.Pos{cm.P{ii, 0}, cm.Down}),
			countEnergized(board, dim, cm.Pos{cm.P{dim - 1, ii}, cm.Left}),
			countEnergized(board, dim, cm.Pos{cm.P{ii, dim - 1}, cm.Up}),
		)
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
