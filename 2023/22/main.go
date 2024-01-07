package main

import (
	"2023/common"
	"fmt"
	"log"
	"strconv"
	"strings"
)

type Brick struct {
	minX, minY, minZ int
	maxX, maxY, maxZ int
}

func Intersect(a, b *Brick) bool {
	disjoint := a.minX > b.maxX || b.minX > a.maxX || a.minY > b.maxY || b.minY > a.maxY
	return !disjoint
}

func Parse(s string, brickStats *Brick) Brick {
	fromInt := func(s string) int {
		res, err := strconv.Atoi(s)
		if err != nil {
			log.Fatal(err)
		}
		return res
	}

	tokens := strings.Split(s, "~")
	mins := strings.Split(tokens[0], ",")
	maxs := strings.Split(tokens[1], ",")
	res := Brick{
		minX: fromInt(mins[0]), minY: fromInt(mins[1]), minZ: fromInt(mins[2]),
		maxX: fromInt(maxs[0]), maxY: fromInt(maxs[1]), maxZ: fromInt(maxs[2]),
	}

	brickStats.minX = min(brickStats.minX, res.minX)
	brickStats.minY = min(brickStats.minY, res.minY)
	brickStats.minZ = min(brickStats.minZ, res.minZ)
	brickStats.maxX = max(brickStats.maxX, res.maxX)
	brickStats.maxY = max(brickStats.maxY, res.maxY)
	brickStats.maxZ = max(brickStats.maxZ, res.maxZ)

	return res
}

type BricksByLevel = []map[int]bool

func MoveDown(byLevel BricksByLevel, bricks []Brick) map[int]bool {
	res := make(map[int]bool)

	for zz := 1; zz < len(byLevel); zz++ {
		shouldGo := true
		for shouldGo {
			shouldGo = false

			for k, _ := range byLevel[zz] {
				zzz := zz
				goodToMove := true
				for goodToMove && zzz > 1 {
					goodToMove = true
					for kk, _ := range byLevel[zzz-1] {
						if Intersect(&bricks[k], &bricks[kk]) {
							goodToMove = false
							break
						}
					}
					if goodToMove {
						zzz--
					}
				}
				if zzz != zz {
					delete(byLevel[zz], k)
					byLevel[zzz][k] = true
					res[k] = true
					shouldGo = true
					break
				}
			}
		}
	}

	return res
}

func Clone(byLevel BricksByLevel) BricksByLevel {
	res := make([]map[int]bool, len(byLevel))
	for ii := range res {
		res[ii] = make(map[int]bool)
		for k, v := range byLevel[ii] {
			res[ii][k] = v
		}
	}
	return res
}

func main() {
	data := common.ReadLines("22/input.txt")
	var brickStats Brick
	nn := len(data)
	bricks := make([]Brick, nn)
	for ii := 0; ii < nn; ii++ {
		bricks[ii] = Parse(data[ii], &brickStats)
	}

	byLevel := make(BricksByLevel, brickStats.maxZ+1)
	for ii := 0; ii < len(byLevel); ii++ {
		byLevel[ii] = make(map[int]bool)
	}

	for ii, br := range bricks {
		for zz := br.minZ; zz <= br.maxZ; zz++ {
			byLevel[zz][ii] = true
		}
	}

	MoveDown(byLevel, bricks)

	easy, hard := 0, 0
	for ii := 0; ii < nn; ii++ {
		cloned := Clone(byLevel)
		for zz := 1; zz < len(cloned); zz++ {
			delete(cloned[zz], ii)
		}
		cnt := len(MoveDown(cloned, bricks))
		hard += cnt
		if cnt == 0 {
			easy++
		}
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
