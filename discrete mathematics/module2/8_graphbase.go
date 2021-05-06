package main

import "fmt"

type Vertex struct {
	Edges []int
	T1    int
	Comp  int
	Low   int
}

func tarjan(graph []Vertex) int {
	var stack []int
	var time, count = 1, 1

	var visitVertex func(int)
	visitVertex = func(index int) {
		graph[index].T1 = time
		graph[index].Low = time
		time++

		stack = append(stack, index)
		for _, uIndex := range graph[index].Edges {
			if graph[uIndex].T1 == 0 {
				visitVertex(uIndex)
			}

			if graph[uIndex].Comp == 0 && graph[index].Low > graph[uIndex].Low {
				graph[index].Low = graph[uIndex].Low
			}
		}
		if graph[index].T1 == graph[index].Low {
			var vIndex int
			for {
				vIndex = stack[len(stack)-1]
				stack = stack[:len(stack)-1]
				graph[vIndex].Comp = count
				if index == vIndex {
					break
				}
			}
			count++
		}
	}
	for i := 0; i < len(graph); i++ {
		if graph[i].T1 == 0 {
			visitVertex(i)
		}
	}

	return count
}

func main() {
	var n, m int
	fmt.Scan(&n)
	fmt.Scan(&m)
	graph := make([]Vertex, n)
	for i := 0; i < m; i++ {
		var indexA, indexB int
		fmt.Scanf("%d %d", &indexA, &indexB)
		graph[indexA].Edges = append(graph[indexA].Edges, indexB)
	}

	count := tarjan(graph)

	condensationsMinimalIndices := make([]int, count)
	useCondensationInBase := make([]bool, count)
	for i := 0; i < count; i++ {
		useCondensationInBase[i] = true
		condensationsMinimalIndices[i] = -1
	}

	for i, vertex := range graph {
		if condensationsMinimalIndices[vertex.Comp] == -1 {
			condensationsMinimalIndices[vertex.Comp] = i
		}
	}

	for _, vertex := range graph {
		for _, uIndex := range vertex.Edges {
			uComp := graph[uIndex].Comp
			if vertex.Comp != uComp {
				useCondensationInBase[uComp] = false
			}
		}
	}

	for i := 1; i < count; i++ {
		if useCondensationInBase[i] == true {
			fmt.Print(condensationsMinimalIndices[i], " ")
		}
	}
	fmt.Println()
}
