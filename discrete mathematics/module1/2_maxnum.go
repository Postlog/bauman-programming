package main

import (
	"fmt"
	"sort"
	"strconv"
)

type StringArray []string

func (array StringArray) Len() int { return len(array) }

func (array StringArray) Less(i, j int) bool {
	a, b := array[i], array[j]
	num1, _ := strconv.Atoi(a + b)
	num2, _ := strconv.Atoi(b + a)
	return num1 > num2
}

func (array StringArray) Swap(i, j int) {
	array[i], array[j] = array[j], array[i]
}

func main() {
	var n int
	fmt.Scan(&n)

	strings := make([]string, n)
	for i := 0; i < n; i++ {
		fmt.Scan(&strings[i])
	}

	sort.Sort(StringArray(strings))
	for _, str := range strings {
		fmt.Print(str)
	}
}
