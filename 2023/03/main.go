package main

import (
	"fmt"
	"log"
	"os"
	"strings"
	"unicode"
)

func main() {
	content, err := os.ReadFile("03/input.txt")
	if err != nil {
		log.Fatal(err)
	}
	board := strings.Split(string(content), "\n")

	rows, cols := len(board), len(board[0])

	curColor := 0
	colors := make([][]int, rows)
	color2num := make(map[int]int)
	for rr := 0; rr < rows; rr++ {
		colors[rr] = make([]int, cols)
		for cc := 0; cc < cols; cc++ {
			colors[rr][cc] = -1
		}
	}

	easy := 0

	checkSymbol := func(rr int, cc int) bool {
		for dr := -1; dr <= 1; dr++ {
			for dc := -1; dc <= 1; dc++ {
				if dc == 0 && dr == 0 {
					continue
				}
				new_r, new_c := rr+dr, cc+dc
				if new_r >= 0 && new_r < rows && new_c >= 0 && new_c < cols {
					ch := rune(board[new_r][new_c])
					if !unicode.IsDigit(ch) && ch != '.' {
						return true
					}
				}
			}
		}

		return false
	}

	for rr := 0; rr < rows; rr++ {
		cc := 0
		for cc < cols {
			num := 0
			hasSymbol := false
			end_cc := cc
			for end_cc < cols {
				ch := rune(board[rr][end_cc])
				if unicode.IsDigit(ch) {
					num = num*10 + int(ch-'0')
					colors[rr][end_cc] = curColor
					hasSymbol = hasSymbol || checkSymbol(rr, end_cc)
					end_cc++
				} else {
					break
				}
			}
			if end_cc > cc {
				if hasSymbol {
					easy += num
				}
				color2num[curColor] = num
				curColor++
				cc = end_cc
			} else {
				cc++
			}
		}
	}

	hard := 0
	for rr := 0; rr < rows; rr++ {
		for cc := 0; cc < cols; cc++ {
			if board[rr][cc] == '*' {
				adj := make(map[int]int)
				for dr := -1; dr <= 1; dr++ {
					for dc := -1; dc <= 1; dc++ {
						if clr := colors[rr+dr][cc+dc]; clr >= 0 {
							adj[clr]++
						}
					}
				}
				if len(adj) == 2 {
					prod := 1
					for k, _ := range adj {
						prod *= color2num[k]
					}
					hard += prod
				}
			}
		}
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
