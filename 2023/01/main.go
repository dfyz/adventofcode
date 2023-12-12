package main

import (
	"2023/common"
	"fmt"
	"strings"
	"unicode"
)

func extractCalibrationValue(s string, checker func(s string, ii int) int) int {
	firstNum, lastNum := -1, -1
	for ii := 0; ii < len(s); ii++ {
		if num := checker(s, ii); num > 0 {
			if firstNum < 0 {
				firstNum = num
			}
			lastNum = num
		}
	}
	return firstNum*10 + lastNum
}

func checkEasy(s string, ii int) int {
	ch := s[ii]
	if unicode.IsDigit(rune(ch)) {
		return int(ch - '0')
	}
	return 0
}

func checkHard(s string, ii int) int {
	if easyNum := checkEasy(s, ii); easyNum > 0 {
		return easyNum
	}
	vals := []string{
		"one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
	}
	cur := s[ii:]
	for jj, vv := range vals {
		if strings.HasPrefix(cur, vv) {
			return jj + 1
		}
	}
	return 0
}

func main() {
	lines := common.ReadLines("01/input.txt")
	easy, hard := 0, 0
	for _, l := range lines {
		easy += extractCalibrationValue(l, checkEasy)
		hard += extractCalibrationValue(l, checkHard)
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
