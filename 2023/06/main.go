package main

import (
	"2023/common"
	"fmt"
	"log"
	"strconv"
	"strings"
	"unicode"
)

func parseInts(s string) []uint64 {
	res := make([]uint64, 0)
	for _, rawVal := range strings.Fields(s)[1:] {
		val, err := strconv.ParseUint(rawVal, 10, 0)
		if err != nil {
			log.Fatal(err)
		}
		res = append(res, val)
	}
	return res
}

func parseMega(s string) uint64 {
	res := uint64(0)
	for _, ch := range s {
		if unicode.IsDigit(ch) {
			res = res*10 + uint64(ch-'0')
		}
	}
	return res
}

func main() {
	lines := common.ReadLines("06/input.txt")

	times := parseInts(lines[0])
	distances := parseInts(lines[1])

	times = append(times, parseMega(lines[0]))
	distances = append(distances, parseMega(lines[1]))

	easy := uint64(1)
	hard := uint64(0)

	for ii, _ := range times {
		t, d := times[ii], distances[ii]
		winCount := uint64(0)
		for xx := uint64(0); xx <= t; xx++ {
			cand := xx * (t - xx)
			if cand > d {
				winCount++
			}
		}
		if ii+1 < len(times) {
			easy *= winCount
		} else {
			hard = winCount
		}
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
