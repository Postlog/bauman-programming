package main

import (
	"fmt"
	"sort"
	"strconv"
)

type StringNumber struct {
	Value string
}

type StringNumbers []StringNumber

func(a StringNumbers) Len() int { return len(a) }
func(a StringNumbers) Less(i, j int) bool { 
	i1, _ := strconv.Atoi(a[i].Value + a[j].Value)
	i2, _ := strconv.Atoi(a[j].Value + a[i].Value)
	return i1 > i2
}
func(a StringNumbers) Swap(i, j int) { a[i], a[j] = a[j], a[i] }

func main() {
	var n int
	fmt.Scan(&n)

	a := make([]StringNumber, n)
  	for i := 0; i < n; i++ {
    	fmt.Scan(&a[i].Value)
    }

    sort.Sort(StringNumbers(a))
    for _, e := range a {
    	fmt.Print(e.Value)
    }
}