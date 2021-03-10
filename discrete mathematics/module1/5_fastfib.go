package main

import (
	"fmt"
	"math/big"
)

func getMatrix() [][]*big.Int {
	M := [][]*big.Int{}
	row1 := []*big.Int{big.NewInt(1), big.NewInt(1)}
	row2 := []*big.Int{big.NewInt(1), big.NewInt(0)}
	M = append(M, row1)
	M = append(M, row2)

	return M
}

func fibonacci(n int) *big.Int {
	F := power(getMatrix(), n-1)

	return F[0][0]
}

func power(F [][]*big.Int, n int) [][]*big.Int {

	if n < 2 {
		return F
	}

	F = power(F, n/2)
	F = multiply(F, F)

	if n%2 != 0 {
		F = multiply(F, getMatrix())
	}

	return F
}

func multiply(F [][]*big.Int, S [][]*big.Int) [][]*big.Int {
	R := [][]*big.Int{}

	for i := 0; i < len(F); i++ {
		R = append(R, []*big.Int{})
		for j := 0; j < len(S[0]); j++ {
			R[i] = append(R[i], big.NewInt(0))
		}
	}

	for i := 0; i < len(F); i++ {
		for j := 0; j < len(S[0]); j++ {
			for k := 0; k < len(F[0]); k++ {
				var t = new(big.Int)
				R[i][j].Add(R[i][j], t.Mul(F[i][k], S[k][j]))
			}
		}
	}

	return R
}

func main() {
	var n int
	fmt.Scan(&n)
	fmt.Println(fibonacci(n))
}
