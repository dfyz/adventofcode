package main

import (
	cm "2023/common"
	"container/heap"
	"fmt"
	"math"
)

type PQItem struct {
	pos   cm.Pos
	dist  int
	index int // priority queue bookkeeping
}

type PQ []*PQItem

func (pq PQ) Len() int             { return len(pq) }
func (pq PQ) Less(ii, jj int) bool { return pq[ii].dist < pq[jj].dist }
func (pq PQ) Swap(ii, jj int) {
	pq[ii], pq[jj] = pq[jj], pq[ii]
	pq[ii].index, pq[jj].index = pq[jj].index, pq[ii].index
}
func (pq *PQ) Push(rawItem any) {
	item := rawItem.(*PQItem)
	item.index = pq.Len()
	*pq = append(*pq, item)
}
func (pq *PQ) Pop() any {
	last := len(*pq) - 1
	res := (*pq)[last]
	*pq = (*pq)[:last]
	return res
}

func solve(board []string, dim int, minSteps, maxSteps int) int {
	startPos := []cm.Pos{
		{cm.P{0, 0}, cm.Right},
		{cm.P{0, 0}, cm.Down},
	}

	pq := make(PQ, len(startPos))
	pqRef := make(map[cm.Pos]*PQItem)
	done := make(map[cm.Pos]bool)

	for ii, pos := range startPos {
		item := &PQItem{pos: pos, dist: 0, index: ii}
		pq[ii] = item
		pqRef[pos] = item
	}
	heap.Init(&pq)

	res := math.MaxInt
	for pq.Len() > 0 {
		best := heap.Pop(&pq).(*PQItem)
		cc, dd := best.pos.Coords, best.pos.Dir
		if cc.Y == dim-1 && cc.X == dim-1 {
			res = min(res, best.dist)
		}

		done[best.pos] = true

		edgeCost := 0
		for delta := 0; delta <= maxSteps; delta++ {
			if delta >= minSteps {
				adjPos := []cm.Pos{
					{cc, cm.RotL(dd)},
					{cc, cm.RotR(dd)},
				}
				for _, adj := range adjPos {
					if done[adj] {
						continue
					}
					newDist := best.dist + edgeCost
					var item *PQItem
					if foundItem, ok := pqRef[adj]; ok {
						item = foundItem
						if newDist < item.dist {
							item.dist = newDist
							heap.Fix(&pq, item.index)
						}
					} else {
						item = &PQItem{pos: adj, dist: newDist}
						heap.Push(&pq, item)
					}
					pqRef[adj] = item
				}
			}

			cc = cc.Add(cm.Dirs[dd])
			if cc.X < 0 || cc.Y < 0 || cc.X >= dim || cc.Y >= dim {
				break
			}
			edgeCost += int(board[cc.Y][cc.X] - '0')
		}
	}
	return res
}

func main() {
	board := cm.ReadLines("17/input.txt")
	dim := len(board)
	cm.Ensure(dim == len(board[0]), "expected a square board")

	fmt.Printf("easy: %d\n", solve(board, dim, 1, 3))
	fmt.Printf("hard: %d\n", solve(board, dim, 4, 10))
}
