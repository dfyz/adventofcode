package main

import (
	"2023/common"
	"fmt"
	"log"
	"slices"
	"sort"
	"strconv"
	"strings"
)

type Hand struct {
	cards string
	bid   int
}

const JOKER = 'B'

func getType(cards string, isEasy bool) float64 {
	countMap := make(map[rune]int)
	jokers := 0
	for _, ch := range cards {
		countMap[ch]++
		if !isEasy && ch == JOKER {
			jokers++
		}
	}

	counts := make([]int, len(countMap))
	ii := 0
	for _, v := range countMap {
		counts[ii] = v
		ii++
	}
	sort.Ints(counts)
	slices.Reverse(counts) // for easier visual inspection

	switch {
	case slices.Equal(counts, []int{5}):
		return 5
	case slices.Equal(counts, []int{4, 1}):
		if jokers == 1 || jokers == 4 {
			return 5
		}
		return 4
	case slices.Equal(counts, []int{3, 2}):
		if jokers == 2 || jokers == 3 {
			return 5
		}
		return 3.5
	case slices.Equal(counts, []int{3, 1, 1}):
		if jokers == 1 || jokers == 3 {
			return 4
		}
		return 3
	case slices.Equal(counts, []int{2, 2, 1}):
		if jokers == 2 {
			return 4
		} else if jokers == 1 {
			return 3.5
		}
		return 2
	case slices.Equal(counts, []int{2, 1, 1, 1}):
		if jokers == 1 || jokers == 2 {
			return 3
		}
		return 1
	default:
		if jokers == 1 {
			return 1
		}
		return 0
	}
}

func Solve(isEasy bool) int {
	hands := make([]Hand, 0)
	for _, line := range common.ReadLines("07/input.txt") {
		tokens := strings.Fields(line)
		bid, err := strconv.Atoi(tokens[1])
		if err != nil {
			log.Fatal(err)
		}
		cards := ""
		for _, ch := range tokens[0] {
			newCh := ch
			switch ch {
			case 'T':
				newCh = 'A'
			case 'J':
				newCh = JOKER
			case 'Q':
				newCh = 'C'
			case 'K':
				newCh = 'D'
			case 'A':
				newCh = 'E'
			}
			cards += string(newCh)
		}
		hands = append(hands, Hand{
			cards,
			bid,
		})
	}

	sort.Slice(hands, func(ii, jj int) bool {
		a, b := hands[ii].cards, hands[jj].cards
		aType, bType := getType(a, isEasy), getType(b, isEasy)
		if aType != bType {
			return aType < bType
		}
		if isEasy {
			return a < b
		}
		remapJoker := func(s string) string {
			return strings.Replace(s, string(JOKER), "1", -1)
		}
		return remapJoker(a) < remapJoker(b)
	})

	res := 0
	for rank, hh := range hands {
		res += (rank + 1) * hh.bid
	}
	return res
}

func main() {
	fmt.Printf("easy: %d\n", Solve(true))
	fmt.Printf("hard: %d\n", Solve(false))
}
