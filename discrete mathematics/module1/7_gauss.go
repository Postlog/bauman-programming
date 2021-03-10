package main

import (
	"fmt"
	"math"
)

func abs(a int) int {
	if a < 0 {
		return -a
	}

	return a
}

func max(a, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

func gcd(a, b int) int {
	a, b = abs(a), abs(b)

	for b != 0 {
		t := b
		b = a % b
		a = t
	}

	return a
}

func lcm(a, b int) int {
	a, b = abs(a), abs(b)

	return (a * b) / gcd(a, b)
}

type Frac struct {
	a, b int
}

func (f Frac) normalize() Frac {
	a, b := f.a, f.b

	if a < 0 && b < 0 {
		a, b = -a, -b
	} else if a < 0 || b < 0 {
		a, b = -abs(a), abs(b)
	}

	k := gcd(a, b)
	a /= k
	b /= k
	return Frac{a, b}
}

func (f1 Frac) add(f2 Frac) Frac {
	a1, b1, a2, b2 := f1.a, f1.b, f2.a, f2.b
	k := max(1, lcm(b1, b2))
	numerator1 := a1 * (k / b1)
	numerator2 := a2 * (k / b2)
	return Frac{numerator1 + numerator2, k}.normalize()
}

func (f1 Frac) substract(f2 Frac) Frac {
	return f1.add(Frac{-f2.a, f2.b}).normalize()
}

func (f1 Frac) divide(f2 Frac) Frac {
	return Frac{f1.a * f2.b, f1.b * f2.a}.normalize()
}

func (f1 Frac) multiply(f2 Frac) Frac {
	return Frac{f1.a * f2.a, f1.b * f2.b}.normalize()
}

func (f1 Frac) asFloat64() float64 {
	return float64(f1.a) / float64(f1.b)
}

func fracify(m [][]int, n int) [][]Frac {
	var fracm [][]Frac

	for i := 0; i < n; i++ {
		fracm = append(fracm, []Frac{})
		for j := 0; j < n+1; j++ {
			fracm[i] = append(fracm[i], Frac{m[i][j], 1})
		}
	}

	return fracm
}

func triangulate(m [][]Frac, n int) ([]int, bool) {
	index := make([]int, n)
	for i := range index {
		index[i] = i
	}

	for i := 0; i < n; i++ {
		primary := m[i][index[i]]

		if primary.a == 0 {
			var primary_i int
			for j := i; j < n; j++ {
				if math.Abs(m[i][index[j]].asFloat64()) > math.Abs(primary.asFloat64()) {
					primary_i = j
				}
			}

			if primary_i > 0 {
				index[i], index[primary_i] = index[primary_i], index[i]
			}

			primary = m[i][index[i]]
		}

		if primary.a == 0 {
			return nil, false
		}

		for j := 0; j < n; j++ {
			m[i][index[j]] = m[i][index[j]].divide(primary)
		}

		m[i][n] = m[i][n].divide(primary)

		for k := i + 1; k < n; k++ {
			primary = m[k][index[i]]
			for j := 0; j < n; j++ {
				m[k][index[j]] = m[k][index[j]].substract(m[i][index[j]].multiply(primary))
			}
			m[k][n] = m[k][n].substract(m[i][n].multiply(primary))
		}
	}

	return index, true
}

func get_answer(m [][]Frac, index []int, n int) []Frac {
	var answer []Frac

	for i := 0; i < n; i++ {
		answer = append(answer, Frac{0, 1})
	}

	for i := n - 1; i >= 0; i-- {
		answer[index[i]] = m[i][n]
		for j := i + 1; j < n; j++ {
			answer[index[i]] = answer[index[i]].substract(answer[index[j]].multiply(m[i][index[j]]))
		}
	}

	return answer
}

func solve(matrix [][]int, n int) []Frac {
	frac_matrix := fracify(matrix, n)

	index, ok := triangulate(frac_matrix, n)
	if !ok {
		return nil
	}

	return get_answer(frac_matrix, index, n)
}

func main() {

	var n int
	fmt.Scan(&n)

	var m [][]int

	for i := 0; i < n; i++ {
		m = append(m, []int{})
		for j := 0; j < n+1; j++ {
			var v int
			fmt.Scan(&v)
			m[i] = append(m[i], v)
		}
	}

	res := solve(m, n)
	if res == nil {
		fmt.Println("No solution")
	} else {
		for _, f := range res {
			fmt.Printf("%d/%d\n", f.a, f.b)
		}
	}
}
