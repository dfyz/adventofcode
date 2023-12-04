package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	content, err := os.ReadFile("04/input.txt")
	if err != nil {
		log.Fatal(err)
	}

	lines := strings.Split(string(content), "\n")
	easy, hard := 0, 0
	counts := make([]int, len(lines))
	for ii := range counts {
		counts[ii] = 1
	}
	for ii, l := range lines {
		scan := bufio.NewScanner(strings.NewReader(l))
		scan.Split(bufio.ScanWords)
		inExpected, inActual := false, false
		expected, actual := make([]int, 0), make([]int, 0)
		for scan.Scan() {
			tkn := scan.Text()
			switch {
			case tkn == "Card":
				// do nothing
			case strings.HasSuffix(tkn, ":"):
				inExpected = true
			case tkn == "|":
				inActual = true
			default:
				num, err := strconv.Atoi(tkn)
				if err != nil {
					log.Fatal(err)
				}
				if inActual {
					actual = append(actual, num)
				} else if inExpected {
					expected = append(expected, num)
				} else {
					log.Fatal("uh oh")
				}
			}
		}

		sort.Ints(expected)
		easyBonus := 0
		matchCount := 0
		for _, a := range actual {
			idx := sort.SearchInts(expected, a)
			if idx < len(expected) && expected[idx] == a {
				matchCount++
				if easyBonus == 0 {
					easyBonus = 1
				} else {
					easyBonus *= 2
				}
			}
		}
		easy += easyBonus
		hard += counts[ii]
		for jj := ii + 1; jj <= ii+matchCount; jj++ {
			counts[jj] += counts[ii]
		}
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
