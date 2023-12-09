package common

import (
	"bufio"
	"log"
	"os"
	"strconv"
	"strings"
)

func ReadLines(fileName string) []string {
	file, err := os.Open(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	res := make([]string, 0)
	sc := bufio.NewScanner(file)
	for sc.Scan() {
		res = append(res, sc.Text())
	}
	return res
}

func ParseInts(line string) []int {
	rawNums := strings.Fields(line)
	nums := make([]int, len(rawNums))
	for ii, rawVal := range rawNums {
		num, err := strconv.Atoi(rawVal)
		if err != nil {
			log.Fatal(err)
		}
		nums[ii] = num
	}
	return nums
}
