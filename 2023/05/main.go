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

const SENTINEL = math.MaxUint64

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

	hard := uint64(SENTINEL)
	for ii := 0; ii < len(seeds); ii += 2 {
		start, length := seeds[ii], seeds[ii+1]
		hard = min(hard, checkHard(start, start+length, maps))
	}
	fmt.Printf("hard: %d\n", hard)
}

func checkHard(start uint64, end uint64, maps [][]Segment) uint64 {
	res := uint64(SENTINEL)
	for start < end {
		cur := start
		delta := uint64(SENTINEL)
		for _, m := range maps {
			for _, seg := range m {
				// A) If we are inside a mapping, map the current point, update the delta, and move on.
				if cur >= seg.src && cur < seg.src+seg.len {
					delta = min(delta, seg.src+seg.len-cur)
					cur = seg.dst + (cur - seg.src)
					break
				} else if cur < seg.src {
					// B) If we are not inside a mapping, move to the nearest mapping start.
					// Note that an update from A) will necessarily be smaller that any of the B) updates.
					// This means that, for simplicity, we can update `delta` here unconditionally.
					delta = min(delta, seg.src-cur)
				}
			}
		}
		res = min(res, cur)
		if delta == SENTINEL {
			// Happens when `start` is located strictly to the right of all mappings.
			// This means the result can't get any smaller, so we should bail out.
			break
		}
		start += delta
	}
	return res
}
