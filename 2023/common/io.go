package common

import (
	"log"
	"os"
	"strings"
)

func ReadLines(fileName string) []string {
	data, err := os.ReadFile(fileName)
	if err != nil {
		log.Fatal(err)
	}
	return strings.Split(string(data), "\n")
}
