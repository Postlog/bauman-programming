package main

import "fmt"

type Vertex struct {
	Edges            []int
	DistancesToRoots []int
}

type Queue struct {
	Storage []int
}

func NewQueue() *Queue {
	return &Queue{[]int{}}
}

func (queue *Queue) Pop() int {
	value := queue.Storage[0]
	queue.Storage = queue.Storage[1:]
	return value
}

func (queue *Queue) Push(value int) {
	queue.Storage = append(queue.Storage, value)
}

func (queue Queue) IsEmpty() bool {
	return len(queue.Storage) == 0
}

func calculateDistancesBFS(graph []Vertex, rootIndex, rootRelativeIndex int) {
	queue := NewQueue()
	queue.Push(rootIndex)
	visited := make([]bool, len(graph))

	for !queue.IsEmpty() {
		index := queue.Pop()
		visited[index] = true
		for _, uIndex := range graph[index].Edges {
			if visited[uIndex] == true {
				continue
			}
			visited[uIndex] = true
			graph[uIndex].DistancesToRoots[rootRelativeIndex] = graph[index].DistancesToRoots[rootRelativeIndex] + 1
			queue.Push(uIndex)
		}
	}
}

func isEquidistant(graph []Vertex, index int) bool {
	for j := 0; j < len(graph[index].DistancesToRoots)-1; j++ {
		if graph[index].DistancesToRoots[j] != graph[index].DistancesToRoots[j+1] || graph[index].DistancesToRoots[j] == 0 {
			return false
		}
	}
	return true
}

func main() {
	var n, m int
	fmt.Scanf("%d\n%d", &n, &m)

	graph := make([]Vertex, n)

	for i := 0; i < m; i++ {
		var indexA, indexB int
		fmt.Scanf("%d %d", &indexA, &indexB)
		graph[indexA].Edges = append(graph[indexA].Edges, indexB)
		graph[indexB].Edges = append(graph[indexB].Edges, indexA)
	}

	var k int
	fmt.Scan(&k)

	for i := 0; i < n; i++ {
		graph[i].DistancesToRoots = make([]int, k)
	}

	for i := 0; i < k; i++ {
		var index int
		fmt.Scan(&index)
		calculateDistancesBFS(graph, index, i)
	}

	hasEquidistant := false
	for i := 0; i < n; i++ {
		if isEquidistant(graph, i) {
			fmt.Printf("%d ", i)
			hasEquidistant = true
		}
	}
	if !hasEquidistant {
		fmt.Println("-")
	} else {
		fmt.Println()
	}
}
