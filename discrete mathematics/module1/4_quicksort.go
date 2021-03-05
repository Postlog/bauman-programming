package main

import "fmt"

func qsort(n int, less func(i, j int) bool, swap func(i, j int)) {
	partition := func(lo, hi int) int {
		p := hi
		for j := lo; j < hi; j++ {
			if less(j, p) {
				swap(j, lo)
				lo++
			}
		}
		swap(lo, hi)
		return lo
	}

	var qsort_rec func(lo, hi int)

	qsort_rec = func(lo, hi int) {
		if lo > hi {
			return
		}

		p := partition(lo, hi)
		qsort_rec(lo, p - 1)
		qsort_rec(p + 1, hi)
	}

	qsort_rec(0, n - 1)
}


func main() {
	var n int

	fmt.Scan(&n)

	a := make([]int, n)
  	for i := 0; i < n; i++ {
    	fmt.Scan(&a[i])
    }

    qsort(len(a), func(i, j int) bool { return a[i] < a[j] }, func(i, j int) { a[i], a[j] = a[j], a[i] })

	for _, e := range a {
		fmt.Printf("%d ", e)
	}
}