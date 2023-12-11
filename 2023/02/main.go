package main

import (
	"2023/common"
	"fmt"
	"log"
	"strings"
)

func main() {
	lines := common.ReadLines("02/input.txt")
	easy := 0
	hard := 0
	for _, l := range lines {
		tokens := strings.Split(l, ": ")
		var gameNo int
		_, err := fmt.Sscanf(tokens[0], "Game %d", &gameNo)
		if err != nil {
			log.Fatal(err)
		}
		sets := strings.Split(tokens[1], "; ")
		goodGame := true
		maxRed, maxGreen, maxBlue := 0, 0, 0
		for _, ss := range sets {
			for _, cube := range strings.Split(ss, ", ") {
				var num int
				var color string
				_, err := fmt.Sscanf(cube, "%d %s", &num, &color)
				if err != nil {
					log.Fatal(err)
				}
				var bound int
				switch color {
				case "red":
					bound = 12
					maxRed = max(maxRed, num)
				case "green":
					bound = 13
					maxGreen = max(maxGreen, num)
				case "blue":
					bound = 14
					maxBlue = max(maxBlue, num)
				default:
					log.Fatalf("Unknown color: %s", color)
				}
				if num > bound {
					goodGame = false
				}
			}
		}
		if goodGame {
			easy += gameNo
		}
		hard += maxRed * maxGreen * maxBlue
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
