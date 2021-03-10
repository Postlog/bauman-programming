package main

import "fmt"

func main() {
    var expression string
	fmt.Scan(&expression)

	calculated := make(map[string]bool)
	var indexStack []int

	runes := []rune(expression)

	for i, el := range runes {
		if el == '(' {
			indexStack = append(indexStack, i)
		} else if el == ')' {
			expressionSlice := string(runes[indexStack[len(indexStack) - 1] : i + 1])
			indexStack = indexStack[:len(indexStack) - 1]
			_, ok := calculated[expressionSlice]
			if !ok {
				calculated[expressionSlice] = true
			}
		}
	}

	fmt.Println(len(calculated))
}