package main

import (
	"2023/common"
	"container/list"
	"fmt"
	"math"
	"strings"
	"unicode"
)

type Item struct {
	name  string
	value int
}

func main() {
	content := common.ReadLines("15/input.txt")[0]
	instrs := strings.Split(content, ",")
	easy := 0
	slots := make([]*list.List, math.MaxUint8+1)
	for _, ins := range instrs {
		hash, slot := uint8(0), uint8(0)
		shouldAdd := false
		curName, curValue := "", 0
		for _, ch := range ins {
			switch {
			case ch == '=' || ch == '-':
				shouldAdd = ch == '='
				slot = hash
			case unicode.IsDigit(ch):
				curValue = int(ch - '0')
			default:
				curName += string(ch)
			}
			hash += uint8(ch)
			hash *= 17
		}
		easy += int(hash)

		lst := &slots[slot]
		if *lst == nil {
			*lst = list.New()
		}
		var e *list.Element
		for e = (*lst).Front(); e != nil; e = e.Next() {
			if e.Value.(*Item).name == curName {
				break
			}
		}
		if shouldAdd {
			if e != nil {
				e.Value.(*Item).value = curValue
			} else {
				(*lst).PushBack(&Item{curName, curValue})
			}
		} else if e != nil {
			(*lst).Remove(e)
		}
	}

	hard := 0
	for ii, slot := range slots {
		if slot != nil {
			idx := 0
			for e := slot.Front(); e != nil; e = e.Next() {
				hard += (ii + 1) * (idx + 1) * e.Value.(*Item).value
				idx++
			}
		}
	}

	fmt.Printf("easy: %d\n", easy)
	fmt.Printf("hard: %d\n", hard)
}
