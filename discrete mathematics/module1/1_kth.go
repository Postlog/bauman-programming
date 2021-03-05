package main

import "fmt"
import "math"

func power(number uint64, pow uint64) uint64 {
	var (
		r uint64 = 1
		i uint64 = 1
	)
	for ; i <= pow; i++ {
		r *= number
	}
	return r
}

func G(i uint64) uint64 {
	if i == 1 {
		return 8
	}

	if i == 19 {
		return math.MaxUint64
	}

	return G(i - 1) + i * 9 * power(10, i - 1)
}

func get_digit_by_index(number uint64, index uint64) uint64 {
	var (
		number_length uint64 = 0
		number_copy uint64 = number
	)


	for ; number_copy > 0; number_copy /= 10 {
		number_length += 1
	}

	return (number / power(10, number_length - index - 1)) % 10
}

func get_digit(k uint64) uint64 {
	var (
		i uint64 = 1
		g = G(i)
	)

	for ; k > g; {
		i += 1
		g = G(i)
	}

	var lowest_g uint64 = 0
	if i > 1 {
		lowest_g = G(i - 1) + 1
	}

	k -= lowest_g
	var count uint64 = k / i
	first_block_number := power(10, i - 1)
	target_number := first_block_number + count
	index := k % i
	return get_digit_by_index(target_number, index)
}


func main() {
	var k uint64
	fmt.Scanf("%d", &k)
	fmt.Println(get_digit(k))

	sort.Sort()
}