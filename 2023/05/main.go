package main

import (
	"2023/common"
	"fmt"
	"log"
	"math"
	"slices"
	"strconv"
	"strings"
)

type Segment struct {
	dst, src, len uint64
}

func ParseSegment(line string) Segment {
	res := Segment{}
	_, err := fmt.Sscanf(line, "%d %d %d", &res.dst, &res.src, &res.len)
	if err != nil {
		log.Fatalf("Failed to parse %s: %v", line, err)
	}
	return res
}

func main() {
	lines := common.ReadLines("05/input.txt")

	seeds := make([]uint64, 0)
	for ii, rawVal := range strings.Fields(lines[0]) {
		if ii > 0 {
			val, err := strconv.ParseUint(rawVal, 10, 0)
			if err != nil {
				log.Fatal(err)
			}
			seeds = append(seeds, val)
		}
	}

	maps := make([][]Segment, 0)
	for ii, line := range lines {
		if strings.Contains(line, "map") {
			maps = append(maps, make([]Segment, 0))
		} else if ii > 0 && len(line) > 0 {
			maps[len(maps)-1] = append(maps[len(maps)-1], ParseSegment(line))
		}
	}

	easySeeds := make([]uint64, len(seeds))
	copy(easySeeds, seeds)
	for _, m := range maps {
		for si, _ := range easySeeds {
			res := easySeeds[si]
			for _, seg := range m {
				if res >= seg.src && res < seg.src+seg.len {
					res = seg.dst + (res - seg.src)
					break
				}
			}
			easySeeds[si] = res
		}
	}

	easy := slices.Min(easySeeds)
	fmt.Printf("easy: %d\n", easy)

	hard := uint64(math.MaxUint64)
	for ii := 0; ii < len(seeds); ii += 2 {
		start, length := seeds[ii], seeds[ii+1]
		hard = min(hard, checkHard(start, start+length, maps))
	}
	fmt.Printf("hard: %d\n", hard)
}

func checkHard(start uint64, end uint64, maps [][]Segment) uint64 {
	res := uint64(math.MaxUint64)
	for start < end {
		cur := start
		delta := uint64(math.MaxUint64)
		for _, m := range maps {
			for _, seg := range m {
				if cur >= seg.src && cur < seg.src+seg.len {
					delta = min(delta, seg.src+seg.len-cur)
					cur = seg.dst + (cur - seg.src)
					break
				}
			}
		}
		res = min(res, cur)
		start += delta
	}
	return res
}
