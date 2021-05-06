package main

import (
	"fmt"
	"sort"
)

func getDividers(number int) []int {
	var dividers []int

	i := 1
	for ; i*i < number; i++ {
		if number%i == 0 {
			dividers = append(dividers, -i)
			dividers = append(dividers, number/-i)
		}
	}
	if i*i == number {
		dividers = append(dividers, -i)
	}

	sort.Ints(dividers)
	return dividers
}

func isEdge(a, b int) bool {
	for i := 2; i <= a/b/2; i++ {
		if (a/b)%i == 0 {
			return false
		}
	}
	return true
}
func main() {
	var number int
	fmt.Scan(&number)
	dividers := getDividers(number)

	fmt.Println("graph {")
	for _, divisor := range dividers {
		fmt.Println("\t", -divisor)
	}

	for i, dividerA := range dividers {
		dividerA = -dividerA
		for _, dividerB := range dividers[i+1:] {
			dividerB = -dividerB
			if dividerA%dividerB == 0 && isEdge(dividerA, dividerB) {
				fmt.Println("\t", dividerA, "--", dividerB)
			}
		}
	}
	fmt.Println("}")
}
