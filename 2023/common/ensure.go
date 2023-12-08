package common

import "log"

func Ensure(cond bool, msg string) {
	if !cond {
		log.Fatalf("Uh oh: %s", msg)
	}
}
