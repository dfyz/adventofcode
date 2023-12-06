package common

import (
	"bufio"
	"log"
	"os"
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
