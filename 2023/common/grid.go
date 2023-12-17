package common

import "image"

const (
	Right = iota
	Down
	Left
	Up
)

var Dirs = []P{
	Right: {1, 0},
	Down:  {0, 1},
	Left:  {-1, 0},
	Up:    {0, -1},
}

type P = image.Point

type Pos struct {
	Coords P
	Dir    int
}

func (self *Pos) Move(newDir int) Pos {
	return Pos{self.Coords.Add(Dirs[newDir]), newDir}
}

func rot(dd int, delta int) int { return (dd + delta + len(Dirs)) % len(Dirs) }
func RotL(dd int) int           { return rot(dd, -1) }
func RotR(dd int) int           { return rot(dd, 1) }
