package main

import (
	"fmt"
	"github.com/skorobogatov/input"
)

func main() {
	s := input.Gets()

	var c1, c2 rune

	input.Scanf("\n%c %c", &c1, &c2)

	i, i1, i2, min_distance := 1, -1, -1, 999998

	var t int

	for _, c := range s {
		if c == c1 {
			if i1 = i; i2 != -1 {
				if t = i1 - i2 - 1; t < min_distance {
					min_distance = t
				}
			}

		} else if c == c2 {
			if i2 = i; i1 != -1 {
				if t = i2 - i1 - 1; t < min_distance {
					min_distance = t
				}
			}
		}

		i++
	}
	fmt.Println(min_distance)
}
